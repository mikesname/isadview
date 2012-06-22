package neo4j.models

import net.liftweb.json.JsonAST.JValue

// Enum definitions
object InstitutionType extends Enumeration(
    "International", "National", "Regional", "Community") {
  type InstitutionType = Value
  val International, National, Regional, Community = Value
}

object PublicationStatus extends Enumeration(
    "Draft", "Published") {
  type PublicationStatus = Value
  val Draft, Published = Value
}

object LevelOfDetail extends Enumeration(
    "Minimal", "Partial", "Complete") {
  type LevelOfDetail = Value
  val Minimal, Partial, Complete = Value
}

object LevelOfDescription extends Enumeration(
    "Collection", "File", "Fonds", "Subfonds", "Series", "Subseries", "Item") {
  type LevelOfDescription = Value
  val Collection, File, Fonds, Subfonds, Series, Subseries, Item = Value
}

case class NoResultsFound(err: String = "") extends Exception
case class MultipleResultsFound(err: String = "") extends Exception

trait JsonBuilder[T] {
  def apply(data: net.liftweb.json.JsonAST.JValue): T
  
  def list(data: JValue): Seq[T] = {
    data.children.map(apply(_))
  }

  def one(data: JValue): T = {
    var l = list(data)
    if (l.length == 0) throw new NoResultsFound()
    else if (l.length > 1) throw new MultipleResultsFound()
    l.head
  }
}

trait Description

trait IdFromUrl {
  private val idMatch = "^.+/(\\d+)$".r
  def url: Option[String]
  lazy val id: Option[Long] = url.map(urlval => {
      urlval match {
        case idMatch(id) => id.toLong
        case _ => -1
      }
    }
  )
}

