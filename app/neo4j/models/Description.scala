package neo4j.models

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

trait Description {
  val id: Long
  def toMap: Map[String,Any]
}

