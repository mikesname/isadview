package neo4j.forms

import play.api.data._
import play.api.data.Forms._

import neo4j.models._

object CollectionForm {

  val form = Form(
    mapping(
      "self" -> text,
      "identity" -> mapping(
        "identifier" -> nonEmptyText,
        "slug" -> nonEmptyText,
        "name" -> nonEmptyText,
        "levelOfDescription" -> optional(number),
        "extentAndMedium" -> optional(text)
      )(CollectionIdentity.apply)(CollectionIdentity.unapply),
      "context" -> mapping(
        "archivalHistory" -> optional(text),
        "acquisition" -> optional(text)
      )(CollectionContext.apply)(CollectionContext.unapply),
      "content" -> mapping(
        "scopeAndContent" -> optional(text),
        "appraisal" -> optional(text),
        "accrurals" -> optional(text),
        "systemOfArrangement" -> optional(text)
      )(CollectionContent.apply)(CollectionContent.unapply),
      "conditions" -> mapping(
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
    )(Collection.apply)(Collection.unapply)
  )
}
