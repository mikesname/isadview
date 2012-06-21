package neo4j.models

object Collection extends JsonInstantiatable[Collection] {
  implicit val formats = net.liftweb.json.DefaultFormats

  def fromJson(data: net.liftweb.json.JsonAST.JValue) = {
    Collection(
      // adjust these as appropriate!
      url = (data \ "self").extractOpt[String],
      identity = CollectionIdentity(
        (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
        (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        (data \ "data" \ "slug").extractOpt[String].getOrElse(""),
        (data \ "data" \ "level_of_description").extractOpt[Int],
        (data \ "data" \ "extent_and_medium").extractOpt[String]
      ),
      context = CollectionContext(
        (data \ "data" \ "archival_history").extractOpt[String],
        (data \ "data" \ "acquisition").extractOpt[String]
      ),
      content = CollectionContent(
        (data \ "data" \ "scope_and_content").extractOpt[String],
        (data \ "data" \ "appraisal").extractOpt[String],
        (data \ "data" \ "accrurals").extractOpt[String]
      ),
      conditions = CollectionConditions(
        (data \ "data" \ "conditions_of_access").extractOpt[String],
        (data \ "data" \ "conditions_of_reproduction").extractOpt[String],
        (data \ "data" \ "languages").extractOpt[String].getOrElse("").split(",").toList,
        (data \ "data" \ "scripts").extractOpt[String].getOrElse("").split(",").toList,
        (data \ "data" \ "physical_characteristics").extractOpt[String],
        (data \ "data" \ "finding_aids").extractOpt[String]
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
        publicationStatus = (data \ "data" \"publication_status").extractOpt[Int].getOrElse(0)
      )
    )
  }
}

case class Collection(
  val identity: CollectionIdentity,
  val context: CollectionContext,
  val content: CollectionContent,
  val conditions: CollectionConditions,
  val materials: CollectionMaterials,
  val control: CollectionControl,
  val admin: CollectionAdmin,
  val url: Option[String] = None
) extends Description {
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
  val slug: String = "",
  val levelOfDescription: Option[Int] = Some(0),
  val extentAndMedium: Option[String] = None
) {
  def otherNames = Nil
  def toMap = Map(
    "identifier" -> identifier,
    "name" -> name,
    "slug" -> slug,
    "level_of_description" -> levelOfDescription,
    "extent_and_medium" -> extentAndMedium
  )
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
