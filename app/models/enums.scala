package models

// Enum definitions
object InstitutionType extends Enumeration(
    "International", "National", "Regional", "Community") {
  type InstitutionType = Value
  val International, National, Regional, Community = Value
}

// Enum definitions
object AuthorityType extends Enumeration(
    "CorporateBody", "Family", "Person") {
  type AuthorityType = Value
  val CorporateBody, Family, Person = Value
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

