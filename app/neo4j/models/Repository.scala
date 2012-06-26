package neo4j.models

import collection.JavaConversions._

object Repository extends Neo4jDataSource[Repository] {
  val indexName = "repository"

  def apply(data: net.liftweb.json.JsonAST.JValue): Repository = {
    Repository(
      id = idFromUrl((data \ "self").extractOpt[String]),
      identity = RepositoryIdentity(
        identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
        name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        slug = (data \ "data" \ "slug").extractOpt[String].getOrElse(""),
        parallelFormsOfName = (data \ "data" \
          "parallel_names").extractOpt[String].getOrElse("").split(",,").toList.filterNot(_.isEmpty),
        otherFormsOfName = (data \ "data" \
          "other_names").extractOpt[String].getOrElse("").split(",,").toList.filterNot(_.isEmpty)
      ),
      contact = Nil,
      description = RepositoryDescription(
        history = (data \ "data" \ "history").extractOpt[String],
        geoculturalContext = (data \ "data" \ "geocultural_context").extractOpt[String]
      ),
      access = RepositoryAccess(
        openingTimes = (data \ "data" \ "opening_times").extractOpt[String],
        conditions = (data \ "data" \ "conditions").extractOpt[String],
        accessibility = (data \ "data" \ "accessibility").extractOpt[String]
      ),
      services = RepositoryServices(
        researchServices = (data \ "data" \ "research_services").extractOpt[String],
        reproductionServices = (data \ "data" \ "reproduction_services").extractOpt[String],
        publicAreas = (data \ "data" \ "public_areas").extractOpt[String]
      ),
      control = RepositoryControl(
        descriptionIdentifier = (data \ "data" \ "description_identifier").extractOpt[String],
        institutionIdentifier = (data \ "data" \ "institution_identifier").extractOpt[String],
        rules = (data \ "data" \ "rules_and_conventions").extractOpt[String],
        status = (data \ "data" \ "status").extractOpt[String],
        levelOfDetail = (data \ "data" \ "level_of_detail").extractOpt[String],
        datesOfCreationRevisionDeletion = (data \ "data" \ "dates").extractOpt[String],
        languagesOfDescription = (data \ "data" \ "languages_of_description").extractOpt[String].getOrElse("").split(",").toList,
        scriptsOfDescription = (data \ "data" \ "scripts_of_description").extractOpt[String].getOrElse("").split(",").toList,
        sources = (data \ "data" \ "sources").extractOpt[String],
        maintainenceNotes = (data \ "data" \ "maintainence_notes").extractOpt[String]
      ),
      admin = RepositoryAdmin(
        publicationStatus = (data \ "data" \"publication_status").extractOpt[Int].getOrElse(0)
      )
    )
  }

  def apply(
    identity: RepositoryIdentity,
    contact: List[Contact],
    description: RepositoryDescription,
    access: RepositoryAccess,
    services: RepositoryServices,
    control: RepositoryControl,
    admin: RepositoryAdmin) = new Repository(
      identity, contact, description, access, services, control, admin
  )

  def formUnapply(repo: Repository) = Option(
    (repo.identity, repo.contact, repo.description, repo.access, repo.services, repo.control, repo.admin))  
}




case class Repository(
  val identity: RepositoryIdentity,
  val contact: List[Contact],
  val description: RepositoryDescription,
  val access: RepositoryAccess,
  val services: RepositoryServices,
  val control: RepositoryControl,
  val admin: RepositoryAdmin,
  val id: Long = -1
) extends Description {

  override def getSubordinateItems = Map(
    "addressOf" -> contact.filterNot(c => c.streetAddress.isEmpty && c.city.isEmpty).map { c =>
      Map(
        "index_name" -> Contact.indexName,
        "data" -> c.toMap)
    }
  )          

  def toMap = {
    identity.toMap ++
    description.toMap ++
    access.toMap ++
    services.toMap ++
    control.toMap ++
    admin.toMap
  }

  def withContacts(contacts: List[Contact]): Repository = {
    Repository(
      identity = identity,
      contact = contacts,
      description = description,
      access = access,
      services = services,
      control = control,
      admin = admin,
      id = id
    )
  }
}

case class RepositoryIdentity(
  val identifier: String = "",
  val slug: String = "",
  val name: String = "",
  val parallelFormsOfName: List[String] = Nil,
  val otherFormsOfName: List[String] = Nil,
  val typeOfEntity: Option[Int] = None 
) {
  def otherNames = Nil
  def toMap = Map(
    "identifier" -> identifier,
    "slug" -> slug,
    "name" -> name
  )
}

case class RepositoryDescription(
  val geoculturalContext: Option[String] = None,
  val history: Option[String] = None,
  val mandates: Option[String] = None,
  val administrativeStructure: Option[String] = None,
  val policies: Option[String] = None,
  val buildings: Option[String] = None,
  val holdings: Option[String] = None,
  val findingAids: Option[String] = None
) {
  def toMap = Map(
    "geocultural_context" -> geoculturalContext,
    "history" -> history,
    "mandates" -> mandates,
    "administrative_structure" -> administrativeStructure,
    "policies" -> policies,
    "buildings" -> buildings,
    "holdings" -> holdings,
    "finding_aids" -> findingAids
  )
}

case class RepositoryAccess(
  val openingTimes: Option[String] = None,
  val conditions: Option[String] = None,
  val accessibility: Option[String] = None
) {
  def toMap = Map(
    "openingTimes" -> openingTimes,
    "conditions" -> conditions,
    "accessibility" -> accessibility
  )
}

case class RepositoryServices(
  val researchServices: Option[String] = None,
  val reproductionServices: Option[String] = None,
  val publicAreas: Option[String] = None
) {
  def toMap = Map(
    "research_services" -> researchServices,
    "reproduction_services" -> reproductionServices,
    "public_areas" -> publicAreas
  )
}

case class RepositoryControl(
  val descriptionIdentifier: Option[String] = None,
  val institutionIdentifier: Option[String] = None,
  val rules: Option[String] = None,
  val status: Option[String] = None,
  val levelOfDetail: Option[String] = None,
  val datesOfCreationRevisionDeletion: Option[String] = None,
  val languagesOfDescription: List[String] = Nil,
  val scriptsOfDescription: List[String] = Nil,
  val sources: Option[String] = None,
  val maintainenceNotes: Option[String] = None
) {
  def toMap = Map(
    "description_identifier" -> descriptionIdentifier,
    "institution_identifier" -> institutionIdentifier,
    "rules" -> rules,
    "status" -> status,
    "level_of_detail" -> levelOfDetail,
    "dates" -> datesOfCreationRevisionDeletion,
    "languages_of_description" -> languagesOfDescription.mkString(","),
    "scripts_of_description" -> scriptsOfDescription.mkString(","),
    "sources" -> sources
  )
}

case class RepositoryAdmin(
  val publicationStatus: Int = 0
) {
  def toMap = Map(
    "publication_status" -> publicationStatus
  )
}

