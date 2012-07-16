package importers

import scala.xml._
import models._
import neo4j.data.Neo4jModel
import com.codahale.jerkson.Json.generate

import app.util.Helpers.slugify

/* Geoff extractor for USHMM Solr dumps, which look extactly
   like Solr 'add' documents. */

object USHMM {

  implicit val locale = new java.util.Locale("en", "US")

  // Reverse lookup of language codes: English -> en
  lazy val languageMap: Map[String,String] = java.util.Locale.getISOLanguages.map(
      code => (app.util.Helpers.languageCodeToName(code), code)).toMap

  private val datePattern = ".+-- (\\d{4})-(\\d{4})\\.?$".r

  // Types of subject
  private val TOPIC = "Topical Term"
  private val PLACE = "Geographic Name"
  private val CORPORATION = "Corporate Name"
  private val PERSON = "Personal Name"

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
  def extractDates(elem: NodeSeq): List[FuzzyDate] = {
    (elem \ "field").filter(attributeValueEquals("subject_heading")).flatMap { f =>
      f.text match {
        case datePattern(start, end) => List(FuzzyDate(start.toInt, end.toInt))
        case _ => Nil
      }
    }.toList
  }

  def createAuthority(ident: String, i: Int, atype: AuthorityType.AuthorityType,
      name: String, role: Option[String] = None, bio: Option[String] = None): List[String] = {
    val item = Authority(atype, name, role, bio)
    val json = generate(item.toMap)
    val idx = generate(filteredMap(item.toMap))
    val desc = slugify(name).replace("-","")
    "(%s)<=|%s| %s".format(desc, Authority.indexName, idx) ::
    "(%s) %s".format(desc, json) ::
    "(%s)-[%sauth%d:%s]->(%s)".format(desc, ident, i, "mentionedIn", ident) :: Nil
  }

  def filteredMap(m: Map[String,Any]) = m.flatMap { case (k, v) =>
    v match {
      case None => Nil
      case _ => List((k, v))
    }
  }

  def extractCreators(ident: String, elem: NodeSeq): List[String] = {
    val names = getFields("creator_name", elem).map(cleanupField)
    val roles = getFields("creator_role", elem).map(cleanupField)
    val bios = getFields("creator_bio", elem).map(cleanupField)

    // not sure how to zip 3 lists together cleanly...
    val combined = names.zip(roles).zip(bios).map(t => (t._1._1, t._1._2, t._2))
    
    combined.zipWithIndex.flatMap { case (details, i) =>
      val (name, role, bios) = details
      val item = Authority(AuthorityType.Person, name, optString(role), optString(bios))
      val json = generate(item.toMap)
      val idx = generate(filteredMap(item.toMap))
      val desc = slugify(name).replace("-","")
      "(%s)<=|%s| %s".format(desc, Authority.indexName, idx) ::
      "(%s) %s".format(desc, json) ::
      "(%s)-[%screated%d:%s]->(%s)".format(ident, ident, i, "createdBy", desc) :: Nil
    }
  }

  def extractSubjects(ident: String, elem: NodeSeq): List[String] = {
    val names = getFields("subject_heading", elem).map(cleanupField)
    val types = getFields("subject_type", elem).map(cleanupField)

    // Places are relatively simple. Create a descriptor for each one and
    // relate it to the model
    val places = names.zip(types).filter(t => t._2 == PLACE).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, stype) = nametype
      val item = Place(name)
      val json = generate(item.toMap)
      val idx = generate(filteredMap(item.toMap))
      val desc = slugify(name).replace("-","")
      "(%s)<=|%s| %s".format(desc, Place.indexName, idx) ::
      "(%s) %s".format(desc, json) ::
      "(%s)-[%splace%d:%s]->(%s)".format(desc, ident, i, "locatesInSpace", ident) :: Nil
    }

    // Subjects are more complicated because they are listed like:
    // WWII -- Holocaust -- Estonia, so we need to make a descriptor
    // for each one.
    // First, get a unique list of topics
    val keywords = names.zip(types).filter(t => t._2 == TOPIC).flatMap { case (name, _) =>
      name.split(" -- ").map(cleanupField)
    }.distinct

    val topics = keywords.zipWithIndex.flatMap { case (keyword, i) =>
      val item = Keyword(keyword)
      val json = generate(item.toMap)
      val idx = generate(filteredMap(item.toMap))
      val desc = slugify(keyword).replace("-","")
      "(%s)<=|%s| %s".format(desc, Keyword.indexName, idx) ::
      "(%s) %s".format(desc, json) ::
      "(%s)-[%skeyword%d:%s]->(%s)".format(desc, ident, i, "describes", ident) :: Nil
    }

    val people = names.zip(types).filter(t => t._2 == PERSON).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.Person, name, None, None)
    }
    val corps = names.zip(types).filter(t => t._2 == CORPORATION).zipWithIndex.flatMap { case (nametype, i) =>
      val (name, _) = nametype
      createAuthority(ident, i, AuthorityType.CorporateBody, name, None, None)
    }
    places ++ topics ++ people ++ corps
  }

  def getParents(ident: String, elem: NodeSeq): List[String] = {
    getField("assoc_parent_irn", elem).map(parent =>
      List("(%s)-[%schild:isChildOf]->(%s)".format(ident, ident, parent))
    ).getOrElse(Nil)
  }

  def extractKeyValues(ident: String, elem: NodeSeq) = {
   Map(
      "identifier"  -> ident,
      "element_type" -> Collection.indexName,
      "name"        -> getField("title", elem),
      "slug"        -> app.util.Helpers.slugify(
          List(getField("title", elem).getOrElse(""), ident).filter(_.nonEmpty).mkString(" ")),
      "source"  -> getFields("acq_source", elem).mkString(", "),
      "source"  -> getFields("provinence", elem).mkString(", "),
      "languages" -> getFields("language", elem).toList.map(name => languageMap.get(name).getOrElse(name)),
      "scope_and_content"  -> getField("scope_content", elem),
      "extent_and_medium"  -> getFields("extent", elem).mkString("\n"),
      "legal_status"  -> getField("legal_status", elem),
      "created_on" -> Collection.nowDateTime,
      "acquisition"  -> multiFields(List("acq_source", "acccession_number", "acq_credit"), "\n", elem)
    )           
  }

  def docToGeoff(repoid: Long, elem: NodeSeq): List[String] = {
    getField("irn", elem).map { ident =>
      val data = extractKeyValues(ident, elem)
      val json = generate(data)
      val idx = generate(filteredMap(data))
      val parents = getParents(ident, elem)
      val subjects = extractSubjects(ident, elem)
      val creators = extractCreators(ident, elem)
      val dates = extractDates(elem).zipWithIndex.flatMap { case(d, i) =>
        "(%sdate%d) %s".format(ident, i, generate(d.toMap)) ::
        //"(%sdate%d)<=|%s| %s".format(ident, i, FuzzyDate.indexName, generate(filteredMap(d.toMap))) ::
        "(%sdate%d)-[%sdate%drel:locatesInTime]->(%s)".format(ident, i, ident, i, ident) :: Nil
      }
      val idxkeys = List("identifier", "name", "slug")
      val reporel = List(
        "(%s)-[%srepository:heldBy]->(repo%d)".format(ident, ident, repoid)
      )

      List(
        "(%s)<=|%s| %s".format(ident, Collection.indexName, idx),
        "(%s) %s".format(ident, json)
      ) ++ dates ++ reporel ++ parents ++ subjects ++ creators
    }.getOrElse(Nil)
  }
}
