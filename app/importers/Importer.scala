package importers

import com.codahale.jerkson.Json

trait Importer[T] {
  implicit val locale = java.util.Locale.getDefault

  /*
   * Filter a map, removing null values and empty strings.
   */
  def filteredMap(m: Map[String,Any]) = m.flatMap { case (k, v) =>
    v match {
      case None => Nil
      case Some("") => Nil
      case "" => Nil
      case _ => List((k, v))
    }
  }

  /*
   * Encapsulates a relationship.
   */
  case class GeoffRelationship(label: String, from: String, to: String) {
    override def toString = "(%s)-[%s%s%s:%s]->(%s)".format(from, from, label, to, label, to)
  }

  /*
   * Encapsulates a Geoff entity, taking care of rendering Geoff
   * statements in the right order or idempotent merging. The index
   * statements go first (stripped of null values) followed by the
   * descriptor.
   */
  case class GeoffEntity(
      val indexName: Option[String] = None,
      val descriptor: String, val data: Map[String,Any]) {
    def toStringList: List[String] = {
      val idxs: List[String] = indexName match {
        case Some(idx) => {
          // We must have a unique index value at the front of the list of index key/pairs
          // so that subsequent items creates in a merge do not overwrite the created node.
          val idxkey = "(%s)<=|%s| %s".format(descriptor, idx, Json.generate(Map("descriptor" -> descriptor)))
          List(idxkey) ++ filteredMap(data).map { case (k, v) =>
            "(%s)<=|%s| %s".format(descriptor, idx, Json.generate(Map(k -> v)))
          }.toList
        }
        case _ => Nil
      }
      idxs ::: "(%s) %s".format(descriptor, Json.generate(data)) :: Nil
    }
  }

  // Reverse lookup of language codes: English -> en
  lazy val languageMap: Map[String,String] = java.util.Locale.getISOLanguages.map(
      code => (app.util.Helpers.languageCodeToName(code), code)).toMap

  def getLanguageCodes(displayNames: List[String]): List[String] = {
    displayNames.map(n => languageMap.getOrElse(n, n))
  }
  
  // FIXME: All hail the temporary solution! Obvs. this should be
  // done in a more rigourous way (or perhaps not at all?)
  val langsToScripts = Map(
    "Latn" -> List("en", "de", "fr", "pl", "sk", "cs", "nl", "it", "hr", "hu", "es", "yi"),
    "Cyrl" -> List("uk", "ru", "sr", "bg", "ro"),
    "Hebr" -> List("he")
  )

  def getScriptCodes(langcodes: List[String]) = langcodes.flatMap { code =>
    langsToScripts.flatMap { case (scode, langs) =>
      if (langs.contains(code)) List(scode) else Nil
    }
  }.distinct

  /*
   * Locate the part in the given document from which to start
   * parsing. In, say, an EAD document with nested sections, this
   * may not be the same as the top-level doc.
   */
  def getEntryPoints(elem: T): List[T] = List(elem)

  /*
   * Abstract data extraction functions.
   */

  /*
   * Extract from `T` the core collection information.
   */
  def extractItem(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract from `T` Geoff statements relating this collection
   * to others in a heirachical manner.
   */
  def extractParents(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract place information.
   */
  def extractPlaces(repoident: String, ident: String, elem: T): List[String]

  /* 
   * Extract people referenced as subject access points
   */
  def extractPeople(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract corporate bodies as subject access points
   */
  def extractCorporateBodies(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract information about subject access points references
   * in this collection.
   */
  def extractSubjects(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract information about the creators of this collection.
   */
  def extractCreators(repoident: String, ident: String, elem: T): List[String]

  /*
   * Extract information about the dates of this collection.
   */
  def extractDates(repoident: String, ident: String, elem: T): List[String]

  /*
   * Get this document's local identifier, scoped to its
   * repository.
   */
  def extractScopedIdentifier(elem: T): Option[String]

  /*
   * Extract and combine all the component parts of an item
   */
  def extractDetails(repoident: String, ident: String, elem: T): List[String] = {
    val collection = extractItem(repoident, ident, elem)
    val parents = extractParents(repoident, ident, elem)
    val places = extractPlaces(repoident, ident, elem)
    val people = extractPeople(repoident, ident, elem)
    val corps = extractCorporateBodies(repoident, ident, elem)
    val subjects = extractSubjects(repoident, ident, elem)
    val creators = extractCreators(repoident, ident, elem)
    val dates = extractDates(repoident, ident, elem)
    collection ++ dates ++ parents ++ places ++ people ++ corps ++ subjects ++ creators
  }


  /*
   * Main entry point. Takes an element of type `T` and returns
   * a set of Geoff statements describing the item.
   */
  def docToGeoff(repoident: String, doc: T): List[String] = {
    getEntryPoints(doc).flatMap { elem =>
      extractScopedIdentifier(elem).map { ident =>
        extractDetails(repoident, ident, elem)
      }.getOrElse(Nil)
    }
  }
}


