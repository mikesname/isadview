package app.models

import org.scala_tools.time.Imports.DateTime


object InstitutionType extends Enumeration(
    "International", "National", "Regional", "Community") {
  type InstitutionType = Value
  val International, National, Regional, Community = Value
}

object LevelOfDetail extends Enumeration(
    "Minimal", "Partial", "Complete") {
  type LevelOfDetail = Value
  val Minumum, Partial, Complete = Value
}

trait ContactType
// TODO: Different types of contact


case class Description(
  val identifier: String,
  val slug: String,
  val name: String
) {
} 

//case class Contact(
//    primary: Boolean = true,
//    person: Option[String],
//    street_address: Option[String],
//    city: Option[String],
//    region: Option[String],
//    postal_code: Option[String],
//    country_code: Option[String],
//    website: Option[String],
//    email: Option[String],
//    telephone: Option[String],
//    fax: Option[String],
//    contact: Option[ContactType],
//    note: Option[String]) extends Description {
//
//}
//
//class Repository (
//    identifier: String,
//    name: String,
//    otherFormsOfName: List[String] = Nil,
//    parallelFormsOfName: List[String] = Nil,
//    typeOfInstitution: Option[InstitutionType.Value] = None,
//    contacts: List[Contact] = Nil,
//    history: Option[String] = None,
//    geographicalContext: Option[String] = None,
//    mandates: Option[String] = None,
//    administrativeStructure: Option[String] = None,
//    collectingPolicies: Option[String] = None,
//    buildings: Option[String] = None,
//    holdings: Option[String] = None,
//    findingAids: Option[String] = None,
//    openingTimes: Option[String] = None,
//    conditionsOfAccess: Option[String] = None,
//    accessibility: Option[String] = None,
//    researchServices: Option[String] = None,
//    reproductionServices: Option[String] = None,
//    publicAreas: Option[String] = None,
//    descriptionIdentifier: Option[String] = None,
//    institutionIdentifier: Option[String] = None,
//    rulesAndConventions: Option[String] = None,
//    status: Option[String] = None,
//    levelOfDetail: Option[LevelOfDetail.Value] = Some(LevelOfDetail.Partial),
//    dates: Option[String] = None,
//    languages: List[String] = Nil,
//    scripts: List[String] = Nil,
//    sources: Option[String] = None,
//    maintenanceNotes: Option[String] = None
//  ) extends Description {
//
//}
