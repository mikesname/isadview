package importers

import models._
import scala.xml._
import app.util.Helpers.slugify

/*
 *  IMPORTANT!  This file currently only supports simple
 *  single-level EAD files describing one collection.
 *  It cannot currently extract a nested heirarchy of
 *  items.
 */

object EAD extends Importer[NodeSeq] with XmlHelper {
  private val datePattern = "\\s*(\\d{4})\\s?-\\s?(\\d{4})\\s*".r
  private val familyPattern = "\\s*(\\w+ family)\\s*".r
  // detect patterns: fl. 1937, 1910-1945, b 1934, fl 1935-1938
  private val authDates = "\\s*((?:fl\\.?|b|c)?\\s*\\d{4}(?:-\\d{4})?)\\s*".r

  private val cPattern = "^c(?:\\d+)$".r


  /*
   * Get all `p` elements in a node and join them together with a
   * double line break.
   */
  def getElementText(elem: NodeSeq, ele: String): Option[String] = {
    optString((elem \ ele \ "p").map(_.text).mkString("\n\n"))
  }

  /*
   * Parse a person specification, such as:
   * Plitt | Henry G | 1918-1993 | Jewish US Major
   *
   * This is complicated by the fact that most elements are optional...
   */
  private def parsePersonString(str: String): Option[Authority] = {

    // FIXME: Dates of existence should be filled, but this requires
    // another simple constructor for Authority types...
    def person(last:String, first:String, dates: Option[String] = None, occupation: Option[String] = None) = 
        Some(Authority(AuthorityType.Person, "%s, %s".format(last, first), dates, occupation))

    def family(name: String) = 
        Some(Authority(AuthorityType.Family, name, None, None))

    val parts = str.split("\\|").map(_.trim).filterNot(_.isEmpty)
    parts match {
      case Array(familyPattern(name)) => family(name)
      case Array(ln, fn, authDates(dates), occ) => person(ln, fn, Some(dates), Some(occ))
      case Array(ln, fn, authDates(dates)) => person(ln, fn, Some(dates))
      case Array(ln, fn) => person(ln, fn)
      // Fall back... Just shove the string in a person name
      case _ => Some(Authority(AuthorityType.Person, str, None, None))
    }
  }

  private def parseCorporateBodyString(str: String): Option[Authority] = {
    val names = str.split("[xX]").toList.map(_.trim).filterNot(_.isEmpty)
    names.headOption.map { name =>
      Authority(
        id = -1, 
        slug = Some(app.util.Helpers.slugify(name)),
        description = AuthorityDescription(
          identity = AuthorityIdentity(
            typeOfEntity = AuthorityType.CorporateBody,
            name = name,
            otherFormsOfName = names.tail
          ),
          description = AuthorityDetails(),
          control = AuthorityControl(),
          admin = AuthorityAdmin()
        )
      )
    }
  }

  private def parsePlace(str: String): Option[Place] = {
    str.split("\\|").map(_.trim).filterNot(_.isEmpty).headOption.map(Place(_))
  }

  // Brute force... enumerate all the different ways dates are repesented
  private val unitDatePattern1 = "(\\d{4})\\s?-\\s?(\\d{4})".r
  private val unitDatePattern2 = "(\\d{4})-\\[(\\d{4})\\]".r
  private val unitDatePattern3 = "(\\d{4}s)-\\[(\\d{4}s)\\]".r
  private val unitDatePattern4 = "\\[(\\d{4})\\]".r
  private val unitDatePattern5 = "(\\d{4})".r
  private val unitDatePattern6 = "(\\d{2})th century".r

  def extractDates(repoident: String, ident: String, elem: NodeSeq) = {
    val dates = (elem \ "did" \ "unitdate").toList.map(_.text).flatMap { text =>
      text match {
        case unitDatePattern1(start, end) => Some(FuzzyDate(start.toInt, end.toInt))
        case unitDatePattern2(start, end) => Some(FuzzyDate(start.toInt, end.toInt))
        case unitDatePattern3(start, end) => Some(FuzzyDate(start.toInt, end.toInt, precision=Some("decade"), circa=Some(true)))
        case unitDatePattern4(start) => Some(FuzzyDate(start.toInt))
        case unitDatePattern5(start) => Some(FuzzyDate(start.toInt))
        case unitDatePattern6(start) => Some(FuzzyDate(start.toInt)) // TODO: Set precision to century
        case _ => None
      }
    }
    dates.flatMap { item =>
      val desc = slugify("%s%s".format(item, ident)).replace("-", "")
      val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("locatesInTime", desc, ident).toString :: Nil
    }
  }

  def extractCreators(repoident: String, ident: String, elem: NodeSeq) = {
    val persons = (elem \ "did" \ "origination" \ "persname").toList.map(_.text)
        .filterNot(_.trim.isEmpty).flatMap(parsePersonString(_))
    val corps = (elem \ "did" \ "origination" \ "corpname").toList.map(_.text)
        .filterNot(_.trim.isEmpty).flatMap(parseCorporateBodyString(_))
    (persons ++ corps).flatMap { item =>
      val desc = slugify(item.name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("createdBy", ident, desc).toString :: Nil
    }
  }

  def extractSubjects(repoident: String, ident: String, elem: NodeSeq) = {
    val kwds = (elem \ "controlaccess" \\ "subject").toList.map(_.text).filterNot(_.trim.isEmpty).map(Keyword(_))
    kwds.flatMap { item =>
      val desc = slugify(item.text).replace("-", "")
      val entity = GeoffEntity(indexName=Some(Keyword.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("describes", desc, ident).toString :: Nil
    }
  }

  def extractPeople(repoident: String, ident: String, elem: NodeSeq) = {
    val auths = (elem \ "controlaccess" \\ "persname").toList.map(_.text).filterNot(_.trim.isEmpty).flatMap(parsePersonString(_))
    auths.flatMap { item =>
      val desc = slugify(item.name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("mentionedIn", desc, ident).toString :: Nil
    }
  }

  def extractCorporateBodies(repoident: String, ident: String, elem: NodeSeq) = {
    val corps = (elem \ "controlaccess" \\ "corpname").toList.map(_.text).flatMap(parseCorporateBodyString(_))
    corps.flatMap { item =>
      val desc = slugify(item.name).replace("-","")
      val entity = GeoffEntity(indexName=Some(Authority.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("mentionedIn", desc, ident).toString :: Nil
    }
  }

  def extractPlaces(repoident: String, ident: String, elem: NodeSeq) = {
    val kwds = (elem \ "controlaccess" \\ "geogname").toList.map(_.text).filterNot(_.trim.isEmpty).map(Place(_))
    kwds.flatMap { item =>
      val desc = slugify(item.text).replace("-", "")
      val entity = GeoffEntity(indexName=Some(Place.indexName), descriptor=desc, data=item.toMap)
      entity.toStringList ::: GeoffRelationship("locatesInSpace", desc, ident).toString :: Nil
    }
  }

  def extractParents(repoident: String, ident: String, elem: NodeSeq) = {
    // TODO: Fix this when importer supports EAD hierarchies
    Nil
  }

  def extractItem(repoident: String, ident: String, elem: NodeSeq) = {
    def getTitle(str: String) = if (!str.trim.isEmpty) str.trim else "Untitled Item " + ident
    def getSlug(str: String) = app.util.Helpers.slugify(str).replaceFirst("^-", "")

    val languages = getLanguageCodes((elem \ "did" \ "langmaterial" \ "language").map(_.text).toList)

    val data = Map(
      "identifier" -> (elem \ "did" \ "unitid").text,
      "name" -> getTitle((elem \ "did" \ "unittitle").text),
      "slug" -> getSlug(getTitle((elem \ "did" \ "unittitle").text)),
      "element_type" -> Collection.indexName,
      "languages_of_description" -> "en", // FIXME
      "scripts_of_description" -> "Latn", // FIXME
      "languages" -> languages.mkString(","),
      "scripts" -> getScriptCodes(languages).mkString(","),
      "created_on" -> Collection.nowDateTime,
      "accruals" -> getElementText(elem, "accruals"),
      "acquisition" -> getElementText(elem, "acqinfo"),
      "appraisal" -> getElementText(elem, "appraisal"),
      "archival_history" -> getElementText(elem, "custodhist"),
      "conditions_of_access" -> getElementText(elem, "accessrestrict"),
      "conditions_of_reproduction" -> getElementText(elem, "userestrict"),
      "extent_and_medium" -> getElementText((elem \ "did" \ "physdesc"), "extent"), // Should be more nuanced!
      "finding_aids" -> getElementText(elem, "otherfindaid"),
      "location_of_copies" -> getElementText(elem, "altformavail"),
      "location_of_originals" -> getElementText(elem, "originalsloc"),
      "physical_characteristics" -> getElementText(elem, "phystech"),
      "rules_and_conventions" -> getElementText(elem, "descrules"),
      "scope_and_content" -> getElementText(elem, "scopecontent"),
      "system_of_arrangement" -> getElementText(elem, "arrangement")
    )

    val entity = GeoffEntity(indexName=Some(Collection.indexName),
        descriptor=ident, data=data)
    val childLevels = elem.head.child.filter(n => n.head.label.matches(cPattern.toString)).flatMap { node =>
      extractScopedIdentifier(node).map { cid =>
        extractDetails(repoident, cid, node) ::: GeoffRelationship("isChildOf", cid, ident).toString :: Nil
      }.getOrElse(Nil)
    }
    val here = entity.toStringList ::: GeoffRelationship("heldBy", ident, repoident).toString :: Nil
    here ++ childLevels
  }

  override def getEntryPoints(elem: NodeSeq): List[NodeSeq] = {
    if ((elem \ "archdesc" \ "did" \ "unitid").length > 0) {
      List(elem \ "archdesc")
    } else {
      (elem \ "archdesc" \ "dsc").head.child.toList.filter(n => n.head.label.matches(cPattern.toString))
    }
  }

  def extractScopedIdentifier(elem: NodeSeq) =
    optString(slugify((elem \ "did" \ "unitid").text).replace("-", ""))
}

