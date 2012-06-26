package neo4j.forms

import play.api.data._
import play.api.data.Forms._

import neo4j.models._

object RepositoryForm {

  val form = Form(
    mapping(
      "identity" -> mapping(
        "identifier" -> nonEmptyText,
        "name" -> nonEmptyText,
        "parallelFormsOfName" -> list(text),
        "otherFormsOfName" -> list(text),
        "typeOfEntity" -> optional(number)
      )(RepositoryIdentity.apply)(RepositoryIdentity.unapply),
      "contact" -> list(mapping(
        "primary" -> boolean,
        "contactPerson" -> optional(text),
        "streetAddress" -> optional(text),
        "city" -> optional(text),
        "region" -> optional(text),
        "postalCode" -> optional(text),
        "countryCode" -> optional(text),
        "telephone" -> optional(text),
        "fax" -> optional(text),
        "email" -> optional(email),
        "website" -> optional(text),
        "note" -> optional(text)
      )(Contact.apply)(Contact.formUnapply)),
      "description" -> mapping(
        "history" -> optional(text),
        "geographicalContext" -> optional(text),
        "mandates" -> optional(text),
        "administrativeStructure" -> optional(text),
        "policies" -> optional(text),
        "buildings" -> optional(text),
        "holdings" -> optional(text),
        "findingAids" -> optional(text)
      )(RepositoryDescription.apply)(RepositoryDescription.unapply),
      "access" -> mapping(
        "openingTimes" -> optional(text),
        "conditions" -> optional(text),
        "accessibility" -> optional(text)
      )(RepositoryAccess.apply)(RepositoryAccess.unapply),
      "services" -> mapping(
        "researchServices" -> optional(text),
        "reproductionServices" -> optional(text),
        "publicAreas" -> optional(text)
      )(RepositoryServices.apply)(RepositoryServices.unapply),
      "control" -> mapping(
        "descriptionIdentifier" -> optional(text),
        "institutionIdentifier" -> optional(text),
        "rules" -> optional(text),
        "status" -> optional(text),
        "levelOfDetail" -> optional(text),
        "datesOfCreationRevisionDeletion" -> optional(text),
        "languagesOfDescription" -> list(text),
        "scriptsOfDescription" -> list(text),
        "mainainenceNotes" -> optional(text),
        "sources" -> optional(text)
      )(RepositoryControl.apply)(RepositoryControl.unapply),
      "admin" -> mapping(
        "publicationStatus" -> number
      )(RepositoryAdmin.apply)(RepositoryAdmin.unapply)
    )(Repository.apply)(Repository.formUnapply)
  )
}
