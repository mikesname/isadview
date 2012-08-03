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

/*
 * Levels of description defined in the EAD schema. Also
 * note that there is much confusion around what these 
 * terms actually mean!
 */
object LevelOfDescription extends Enumeration(
    "class", "collection", "file", "fonds", "item", "otherlevel", "recordgrp",
    "series", "subfonds", "subgrp", "subseries") {
  type Level = Value
  val `class`, collection, file, fonds, item, otherlevel,
      recordgrp, series, subfonds, subgrp, subseries = Value
}


