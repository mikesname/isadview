package importers

import scala.xml._
import models._
import neo4j.data.Neo4jModel
import com.codahale.jerkson.Json.generate

import app.util.Helpers.slugify

/* Geoff extractor for USHMM Solr dumps, which look extactly
   like Solr 'add' documents. */

object USHMM {
  def filteredMap(m: Map[String,Any]) = m.flatMap { case (k, v) =>
    v match {
      case None => Nil
      case Some("") => Nil
      case "" => Nil
      case _ => List((k, v))
    }
  }

  case class GeoffRelationship(label: String, from: String, to: String) {
    override def toString = "(%s)-[%s%s%s:%s]->(%s)".format(from, from, label, to, label, to)
  }

  case class GeoffEntity(
      val indexName: Option[String] = None,
      val descriptor: String, val data: Map[String,Any]) {
    def toStringList: List[String] = {
      val idxs: List[String] = indexName match {
        case Some(idx) => {
          // We must have a unique index value at the front of the list of index key/pairs
          // so that subsequent items creates in a merge do not overwrite the created node.
          val idxkey = "(%s)<=|%s| %s".format(descriptor, idx, generate(Map("descriptor" -> descriptor)))
          List(idxkey) ++ filteredMap(data).map { case (k, v) =>
            "(%s)<=|%s| %s".format(descriptor, idx, generate(Map(k -> v)))
          }.toList
        }
        case _ => Nil
      }
      idxs ::: "(%s) %s".format(descriptor, generate(data)) :: Nil
    }
  }

  implicit val locale = java.util.Locale.getDefault

  // Reverse lookup of language codes: English -> en
  lazy val languageMap: Map[String,String] = java.util.Locale.getISOLanguages.map(
      code => (app.util.Helpers.languageCodeToName(code), code)).toMap

  private val datePattern = ".+-- (\\d{4})-(\\d{4})\\.?$".r

  // Types of subject
  private val TOPIC = "Topical Term"
  private val PLACE = "Geographic Name"
  private val CORPORATION = "Corporate Name"
  private val PERSON = "Personal Name"

  // This isn't wise, perhaps...
  private val DEFAULT_SCRIPT = "Latn"


  def optString(s: String) = if (s.trim.isEmpty) None else Some(s)

  def attributeValueEquals(value: String)(node: Node) = {
    node.attributes.exists(_.value.text == value)
  }

  def getFields(field: String, elem: NodeSeq): List[String] = {
    (elem \ "field").filter(attributeValueEquals(field))
            .map(_.text).toList
  }

  def getField(field: String, elem: NodeSeq): Option[String] = {
    optString((elem \ "field").filter(attributeValueEquals(field)).text)
  }

  def multiFields(fields: List[String], sep: String, elem: NodeSeq) = {
    optString(fields.map { fname =>
      (elem \ "field").filter(attributeValueEquals(fname)).text
    }.filter(_.trim.nonEmpty).mkString("\n"))
  }

  // Strip trailing period from certain fields
  def cleanupField(str: String) = if (str.trim.endsWith(".")) str.trim.init else str.trim

  /* Extract date fields, which in the USHMM dump format are
  the last YYYY-YYYY field on keyword access fields. */
  def extractDates(ident: String, elem: NodeSeq): List[String] = {
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

  def extractCreators(ident: String, elem: NodeSeq): List[String] = {
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

  def extractSubjects(ident: String, elem: NodeSeq): List[String] = {
    val names = getFields("subject_heading", elem).map(cleanupField)
    val types = getFields("subject_type", elem).map(cleanupField)

    def isValid(item: (String,String), itemtype: String) = (!item._1.trim.isEmpty) && item._2 == itemtype

    // Places are relatively simple. Create a descriptor for each one and
    // relate it to the model
    val places = names.zip(types).filter(t => isValid(t, PLACE)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, stype) = nametype
      val item = Place(name)
      val desc = slugify(name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Place.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("locatesInSpace", desc, ident).toString :: Nil
    }

    // Subjects are more complicated because they are listed like:
    // WWII -- Holocaust -- Estonia, so we need to make a descriptor
    // for each one.
    // First, get a unique list of topics
    val keywords = names.zip(types).filter(t => isValid(t, TOPIC)).flatMap { case (name, _) =>
      name.split(" -- ").map(cleanupField).lastOption match {
        case Some(t) => List(t)
        case None => Nil
      }
    }.distinct.filterNot(_.isEmpty)

    val topics = keywords.zipWithIndex.flatMap { case (keyword, i) =>
      val item = Keyword(keyword)
      val desc = slugify(keyword).replace("-","")
      val entity = GeoffEntity(indexName=Some(Keyword.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("describes", desc, ident).toString :: Nil
    }

    val people = names.zip(types).filter(t => isValid(t, PERSON)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.Person, name, None, None)
    }
    val corps = names.zip(types).filter(t => isValid(t, CORPORATION)).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.CorporateBody, name, None, None)
    }
    places ++ topics ++ people ++ corps
  }

  def extractParents(ident: String, elem: NodeSeq): List[String] = {
    getField("assoc_parent_irn", elem).map(parent =>
      List(GeoffRelationship("isChildOf", ident, parent).toString)
    ).getOrElse(Nil)
  }

  def extractKeyValues(ident: String, elem: NodeSeq) = {

    def getLanguage(displayNames: List[String]) = {
      if (displayNames.isEmpty)
        locale.getLanguage
      else
        displayNames.map(n => languageMap.getOrElse(n, n)).mkString(",")
    }

    def getTitle(str: String) = if (!str.trim.isEmpty) str.trim else "Untitled Item " + ident
    def getSlug(str: String) = app.util.Helpers.slugify(str).replaceFirst("^-", "")

    Map(
      "identifier"  -> ident,
      "element_type" -> Collection.indexName,
      "name"        -> getTitle(getField("title", elem).getOrElse("")),
      "slug"        -> getSlug(getField("title", elem).getOrElse(ident)),
      "source"  -> getFields("acq_source", elem).mkString(", "),
      "source"  -> getFields("provinence", elem).mkString(", "),
      "languages" -> getLanguage(getFields("language", elem).toList),
      "scripts" -> DEFAULT_SCRIPT,
      "scope_and_content"  -> getField("scope_content", elem),
      "extent_and_medium"  -> getFields("extent", elem).mkString("\n"),
      "legal_status"  -> getField("legal_status", elem),
      "created_on" -> Collection.nowDateTime,
      "acquisition"  -> multiFields(List("acq_source", "acccession_number", "acq_credit"), "\n", elem)
    )           
  }

  def docToGeoff(repoid: Long, elem: NodeSeq): List[String] = {
    getField("irn", elem).map { ident =>
      val entity = GeoffEntity(indexName=Some(Collection.indexName), descriptor=ident,
          data=extractKeyValues(ident, elem))
      val parents = extractParents(ident, elem)
      val subjects = extractSubjects(ident, elem)
      val creators = extractCreators(ident, elem)
      val dates = extractDates(ident, elem)
      val reporel = List(
        GeoffRelationship("heldBy", ident, "repo%d".format(repoid)).toString
      )
      entity.toStringList ++ dates ++ reporel ++ parents ++ subjects ++ creators
    }.getOrElse(Nil)
  }
}
