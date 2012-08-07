package importers

import scala.xml._
import models._

import app.util.Helpers.slugify

/* Geoff extractor for USHMM Solr dumps, which look extactly
   like Solr 'add' documents. */

object USHMM extends Importer[NodeSeq] with XmlHelper {
  private val datePattern = ".+-- (\\d{4})-(\\d{4})\\.?$".r

  // Types of subject
  private val TOPIC = "Topical Term"
  private val PLACE = "Geographic Name"
  private val CORPORATION = "Corporate Name"
  private val PERSON = "Personal Name"

  // This isn't wise, perhaps...
  private val DEFAULT_SCRIPT = "Latn"

  private def attributeValueEquals(value: String)(node: Node) = {
    node.attributes.exists(_.value.text == value)
  }

  private def getFields(field: String, elem: NodeSeq): List[String] = {
    (elem \ "field").filter(attributeValueEquals(field))
            .map(_.text).toList
  }

  private def getField(field: String, elem: NodeSeq): Option[String] = {
    optString((elem \ "field").filter(attributeValueEquals(field)).text)
  }

  private def multiFields(fields: List[String], sep: String, elem: NodeSeq) = {
    optString(fields.map { fname =>
      (elem \ "field").filter(attributeValueEquals(fname)).text
    }.filter(_.trim.nonEmpty).mkString("\n"))
  }

  // Strip trailing period from certain fields
  def cleanupField(str: String) = if (str.trim.endsWith(".")) str.trim.init else str.trim

  /* Extract date fields, which in the USHMM dump format are
  the last YYYY-YYYY field on keyword access fields. */
  def extractDates(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    val dates = (elem \ "field").filter(attributeValueEquals("subject_heading")).flatMap { f =>
      f.text match {
        case datePattern(start, end) => List(FuzzyDate(start.toInt, end.toInt))
        case _ => Nil
      }
    }.toList

    dates.flatMap { d =>
      // We want individual dates per record, so scope them according to the ident.
      val desc = slugify("%s%s".format(d, ident)).replace("-", "")
      val entity = GeoffEntity(indexName=Some(FuzzyDate.indexName), descriptor=desc, data=d.toMap)
      entity.toStringList ::: GeoffRelationship("locatesInTime", desc, ident).toString :: Nil
    }
  }

  def createAuthority(ident: String, i: Int, atype: AuthorityType.AuthorityType,
      name: String, role: Option[String] = None, bio: Option[String] = None): List[String] = {
    val item = Authority(atype, name, role, bio)
    val desc = slugify(name).replace("-","")
    val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
    entity.toStringList ::: GeoffRelationship("mentionedIn", desc, ident).toString :: Nil
  }

  def extractCreators(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    val names = getFields("creator_name", elem).map(cleanupField)
    val roles = getFields("creator_role", elem).map(cleanupField)
    val bios = getFields("creator_bio", elem).map(cleanupField)

    // not sure how to zip 3 lists together cleanly...
    val combined = names.zip(roles).zip(bios).map(t => (t._1._1, t._1._2, t._2)).filterNot(t => t._1.isEmpty)
    
    combined.zipWithIndex.flatMap { case (details, i) =>
      val (name, role, bios) = details
      val item = Authority(AuthorityType.Person, name, optString(role), optString(bios))
      val desc = slugify(name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("createdBy", ident, desc).toString :: Nil
    }
  }
    
  private def isValid(item: (String,String), itemtype: String) = (!item._1.trim.isEmpty) && item._2 == itemtype

  private def subjectTypeTuple(elem: NodeSeq): List[(String,String)] = {
    val names = getFields("subject_heading", elem).map(cleanupField)
    val types = getFields("subject_type", elem).map(cleanupField)
    names.zip(types)
  }

  def extractPlaces(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    // Places are relatively simple. Create a descriptor for each one and
    // relate it to the model
    subjectTypeTuple(elem).filter(t => isValid(t, PLACE)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, stype) = nametype
      val item = Place(name)
      val desc = slugify(name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Place.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("locatesInSpace", desc, ident).toString :: Nil
    }
  }

  def extractPeople(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    subjectTypeTuple(elem).filter(t => isValid(t, PERSON)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.Person, name, None, None)
    }
  }

  def extractCorporateBodies(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    subjectTypeTuple(elem).filter(t => isValid(t, CORPORATION)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.CorporateBody, name, None, None)
    }
  }

  def extractSubjects(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    // Subjects are more complicated because they are listed like:
    // WWII -- Holocaust -- Estonia, so we need to make a descriptor
    // for each one.
    // First, get a unique list of topics
    val keywords = subjectTypeTuple(elem).filter(t => isValid(t, TOPIC)).flatMap { case (name, _) =>
      name.split(" -- ").map(cleanupField).lastOption match {
        case Some(t) => List(t)
        case None => Nil
      }
    }.distinct.filterNot(_.isEmpty)

    keywords.zipWithIndex.flatMap { case (keyword, i) =>
      val item = Keyword(keyword)
      val desc = slugify(keyword).replace("-","")
      val entity = GeoffEntity(indexName=Some(Keyword.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("describes", desc, ident).toString :: Nil
    }
  }

  def extractParents(repoident: String, ident: String, elem: NodeSeq): List[String] = {
    val pirn = getFields("assoc_parent_irn", elem).map(cleanupField)
    val pobj = getFields("assoc_parent_object", elem).map(cleanupField)
    pirn.zip(pobj).flatMap { case (irn, name) =>
      val slug = slugify(name)
      val item = Collection(irn, slug, name)
      val entity = GeoffEntity(indexName=Some(Collection.indexName), descriptor=irn, data=item.toMap)
      val parentRel = GeoffRelationship("isChildOf", ident, irn).toString
      val parentRepo = GeoffRelationship("heldBy", irn, repoident).toString
      entity.toStringList ::: parentRepo :: parentRel :: Nil
    }
  }

  def extractScopedIdentifier(elem: NodeSeq): Option[String] = getField("irn", elem)

  def extractItem(repoident: String, ident: String, elem: NodeSeq) = {

    def getTitle(str: String) = if (!str.trim.isEmpty) str.trim else "Untitled Item " + ident
    def getSlug(str: String) = app.util.Helpers.slugify(str).replaceFirst("^-", "")

    val data = Map(
      "identifier"  -> ident,
      "element_type" -> Collection.indexName,
      "name"        -> getTitle(getField("title", elem).getOrElse("")),
      "slug"        -> getSlug(getField("title", elem).getOrElse(ident)),
      "source"  -> getFields("acq_source", elem).mkString(", "),
      "administrative_history"  -> getFields("provenance", elem).mkString("\n\n"),
      "languages" -> getLanguageCodes(getFields("language", elem).toList).mkString(","),
      "scripts" -> DEFAULT_SCRIPT,
      "languages_of_description" -> "en",
      "scripts_of_description" -> "Latn",
      "scope_and_content"  -> getField("scope_content", elem),
      "extent_and_medium"  -> getFields("extent", elem).mkString("\n\n"),
      "legal_status"  -> getField("legal_status", elem),
      "created_on" -> Collection.nowDateTime,
      "acquisition"  -> multiFields(List("acq_source", "acccession_number", "acq_credit"), "\n\n", elem)
    )           
    val entity = GeoffEntity(indexName=Some(Collection.indexName), descriptor=ident, data=data)
    entity.toStringList ::: GeoffRelationship("heldBy", ident, repoident).toString :: Nil
  }
}

