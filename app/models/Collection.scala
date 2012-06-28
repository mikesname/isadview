package models

import neo4j.data._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

object Collection extends Neo4jDataSource[Collection] {

  val indexName = "collection" 

  // JSON Constructor...
  def apply(data: net.liftweb.json.JsonAST.JValue): Collection = {
    Collection(
      // adjust these as appropriate!
      id = idFromUrl((data \ "self").extractOpt[String]),
      slug = (data \ "data" \ "slug").extractOpt[String],
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      description = CollectionDescription(
        identity = CollectionIdentity(
          identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
          name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
          levelOfDescription = (data \ "data" \ "level_of_description").extractOpt[Int],
          extentAndMedium = (data \ "data" \ "extent_and_medium").extractOpt[String]
        ),
        context = CollectionContext(
          archivalHistory = (data \ "data" \ "archival_history").extractOpt[String],
          acquisition = (data \ "data" \ "acquisition").extractOpt[String]
        ),
        content = CollectionContent(
          scopeAndContent = (data \ "data" \ "scope_and_content").extractOpt[String],
          appraisal = (data \ "data" \ "appraisal").extractOpt[String],
          accrurals = (data \ "data" \ "accrurals").extractOpt[String]
        ),
        conditions = CollectionConditions(
          conditionsOfAccess = (data \ "data" \ "conditions_of_access").extractOpt[String],
          conditionsOfReproduction = (data \ "data" \ "conditions_of_reproduction").extractOpt[String],
          languages = (data \ "data" \ "languages").extractOpt[String].getOrElse("").split(",").toList,
          scripts = (data \ "data" \ "scripts").extractOpt[String].getOrElse("").split(",").toList,
          physicalCharacteristics = (data \ "data" \ "physical_characteristics").extractOpt[String],
          findingAids = (data \ "data" \ "finding_aids").extractOpt[String]
        ),
        materials = CollectionMaterials(
          (data \ "data" \ "location_of_materials").extractOpt[String],
          (data \ "data" \ "location_of_copies").extractOpt[String],
          (data \ "data" \ "related_units_of_description").extractOpt[String]
        ),
        control = CollectionControl(
          (data \ "data" \ "rules").extractOpt[String],
          (data \ "data" \ "languages_of_description").extractOpt[String].getOrElse("").split(",").toList,
          (data \ "data" \ "scripts_of_description").extractOpt[String].getOrElse("").split(",").toList,
          (data \ "data" \ "sources").extractOpt[String]
        ),
        admin = CollectionAdmin(
          publicationStatus = (data \ "data" \ "publication_status").extractOpt[Int].getOrElse(0)
        )
      )
    )
  }
}

case class Collection(
  val id: Long = -1,
  val slug: Option[String] = None,
  val description: CollectionDescription,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None
) extends Neo4jSlugModel with CrudUrls {
  def name = description.identity.name
  val detailUrl = controllers.routes.Collections.detail(slug=slug.getOrElse(""))
  val editUrl = controllers.routes.Collections.edit(slug=slug.getOrElse(""))
  val deleteUrl = controllers.routes.Collections.confirmDelete(slug=slug.getOrElse(""))

  // FIXME: This is really ugly and reqires knowing way too much about the
  // details of persistance. Ideally we'd just pass a Map of relationship
  // to list of items, but the persister also needs to know the index name
  // for the subordinate items, which is can only get from the companion
  // object class
  override def getSubordinateItems = Map(
    "locatesInTime" -> description.identity.dates.filterNot(_.startDate.isEmpty).map { d =>
      Map(
        "index_name" -> FuzzyDate.indexName,
        "data" -> d.toMap)
    }
  )

  override def getIncomingSubordinateRelations = List("locatesInTime")

  def toMap = {
    Map(
      "slug" -> slug,
      "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
      "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
    ) ++ description.toMap
  }

  def withSlug(newSlug: String) = copy(slug=Some(newSlug))
}

case class CollectionDescription(
  val identity: CollectionIdentity,
  val context: CollectionContext,
  val content: CollectionContent,
  val conditions: CollectionConditions,
  val materials: CollectionMaterials,
  val control: CollectionControl,
  val admin: CollectionAdmin
) {
  def withDates(dates: List[FuzzyDate]) = copy(identity=identity.withDates(dates))
  def toMap = {
    identity.toMap ++
    context.toMap ++
    content.toMap ++
    conditions.toMap ++
    materials.toMap ++
    control.toMap ++
    admin.toMap
  }
}


case class CollectionIdentity(
  val identifier: String = "",
  val name: String = "",
  val dates: List[FuzzyDateDescription] = Nil,
  val levelOfDescription: Option[Int] = Some(0),
  val extentAndMedium: Option[String] = None
) {
  def otherNames = Nil
  def toMap = Map(
    "identifier" -> identifier,
    "name" -> name,
    "level_of_description" -> levelOfDescription,
    "extent_and_medium" -> extentAndMedium
  )
  def withDates(newDates: List[FuzzyDate]) = copy(dates=newDates.map(_.description))
}

case class CollectionContext(
  val archivalHistory: Option[String] = None,
  val acquisition: Option[String] = None
) {
  def toMap = Map(
    "archival_history" -> archivalHistory,
    "acquisition" -> acquisition
  )
}

case class CollectionContent(
  val scopeAndContent: Option[String] = None,
  val appraisal: Option[String] = None,
  val accrurals: Option[String] = None,
  val systemOfArrangement: Option[String] = None
) {
  def toMap = Map(
    "scope_and_content" -> scopeAndContent,
    "appraisal"  -> appraisal,
    "accrurals" -> accrurals,
    "system_of_arrangement" -> systemOfArrangement
  )
}

case class CollectionConditions(
  val conditionsOfAccess: Option[String] = None,
  val conditionsOfReproduction: Option[String] = None,
  val languages: List[String] = Nil,
  val scripts: List[String] = Nil,
  val physicalCharacteristics: Option[String] = None,
  val findingAids: Option[String] = None
) {
  def toMap = Map(
    "conditions_of_access" -> conditionsOfAccess,
    "conditions_of_reproduction" -> conditionsOfReproduction,
    "languages" -> languages.mkString(","),
    "scripts" -> scripts.mkString(","),
    "physical_characteristics" -> physicalCharacteristics,
    "finding_aids" -> findingAids
  )
}

case class CollectionMaterials(
  val locationOfOriginals: Option[String] = None,
  val locationOfCopies: Option[String] = None,
  val relatedUnitsOfDescription: Option[String] = None
) {
  def toMap = Map(
    "location_of_originals" -> locationOfOriginals,
    "location_of_copies" -> locationOfCopies,
    "related_units_of_description" -> relatedUnitsOfDescription
  )
}

case class CollectionNotes() {
  def toMap = Map()
}

case class CollectionControl(
  val rules: Option[String] = None,
  val languagesOfDescription: List[String] = Nil,
  val scriptsOfDescription: List[String] = Nil,
  val sources: Option[String] = None
) {
  def toMap = Map(
    "rules" -> rules,
    "languages_of_description" -> languagesOfDescription.mkString(","),
    "scripts_of_description" -> scriptsOfDescription.mkString(","),
    "sources" -> sources
  )
}

case class CollectionAdmin(
  val publicationStatus: Int = 0
) {
  def toMap = Map(
    "publication_status" -> publicationStatus
  )
}
