package forms

import play.api.data._
import play.api.data.Forms._

import models._

object AuthorityForm {

  val form = Form(
    mapping(
      "identity" -> mapping(
        "typeOfEntity" -> number,
        "identifier" -> nonEmptyText,
        "name" -> nonEmptyText,
        "otherFormsOfName" -> list(text)
      )(AuthorityIdentity.apply)(AuthorityIdentity.unapply),
      "description" -> mapping(
        "datesOfExistence" -> optional(text),
        "history" -> optional(text),
        "places" -> optional(text),
        "functions" -> optional(text),
        "geneology" -> optional(text),
        "generalContext" -> optional(text)
      )(AuthorityDetails.apply)(AuthorityDetails.unapply),
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
      )(AuthorityControl.apply)(AuthorityControl.unapply),
      "admin" -> mapping(
        "publicationStatus" -> number
      )(AuthorityAdmin.apply)(AuthorityAdmin.unapply)
    )(AuthorityDescription.apply)(AuthorityDescription.unapply)
  )
}
