package forms

import play.api.data._
import play.api.data.Forms._

import models._

object CollectionForm {
  val form = Form(
    mapping(
      "identity" -> mapping(
        "identifier" -> nonEmptyText,
        "name" -> nonEmptyText,
        "dates" -> list(mapping(
          "startDate" -> optional(jodaDate("yyyy-MM-dd")),
          "endDate" -> optional(jodaDate("yyyy-MM-dd")),
          "precison" -> optional(text),
          "circa" -> optional(boolean)
          )(FuzzyDateDescription.apply)(FuzzyDateDescription.unapply)
        ),
        "levelOfDescription" -> optional(number),
        "extentAndMedium" -> optional(text)
      )(CollectionIdentity.apply)(CollectionIdentity.unapply),
      "context" -> mapping(
        "archivalHistory" -> optional(text),
        "administrativeHistory" -> optional(text),
        "acquisition" -> optional(text)
      )(CollectionContext.apply)(CollectionContext.unapply),
      "content" -> mapping(
        "scopeAndContent" -> optional(text),
        "appraisal" -> optional(text),
        "accrurals" -> optional(text),
        "systemOfArrangement" -> optional(text)
      )(CollectionContent.apply)(CollectionContent.unapply),
      "conditions" -> mapping(
        "legalStatus" -> optional(text),
        "conditionsOfAccess" -> optional(text),
        "conditionsOfReproduction" -> optional(text),
        "languages" -> list(text),
        "scripts" -> list(text),
        "physicalCharacteristics" -> optional(text),
        "findingAids" -> optional(text)
      )(CollectionConditions.apply)(CollectionConditions.unapply),
      "material" -> mapping(
        "locationOfOriginals" -> optional(text),
        "locationOfCopies" -> optional(text),
        "relatedUnitsOfDescription" -> optional(text)
      )(CollectionMaterials.apply)(CollectionMaterials.unapply),
      "control" -> mapping(
        "rules" -> optional(text),
        "languagesOfDescription" -> list(text),
        "scriptsOfDescription" -> list(text),
        "sources" -> optional(text)
      )(CollectionControl.apply)(CollectionControl.unapply),
      "admin" -> mapping(
        "publicationStatus" -> number
      )(CollectionAdmin.apply)(CollectionAdmin.unapply)
    )(CollectionDescription.apply)(CollectionDescription.unapply)
  )
}
