package neo4j.models

import collection.JavaConversions._

object Repository extends Neo4jDataSource[Repository] {
  val indexName = "repository"

  def apply(data: net.liftweb.json.JsonAST.JValue): Repository = {
    Repository(
      id = idFromUrl((data \ "self").extractOpt[String]),
      slug = (data \ "data" \ "slug").extractOpt[String],
      identity = RepositoryIdentity(
        identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
        name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        parallelFormsOfName = (data \ "data" \
          "parallel_forms_of_name").extractOpt[String].getOrElse("").split(",,").toList.filterNot(_.isEmpty),
        otherFormsOfName = (data \ "data" \
          "other_forms_of_name").extractOpt[String].getOrElse("").split(",,").toList.filterNot(_.isEmpty),
        typeOfEntity = (data \ "data" \ "type_of_entity").extractOpt[Int]
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
      control = AuthorityControl(
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
    control: AuthorityControl,
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
  val control: AuthorityControl,
  val admin: RepositoryAdmin,
  val slug: Option[String] = None,
  val id: Long = -1
) extends CrudDescription {
  val detailUrl = controllers.routes.Repositories.detail(slug=slug.getOrElse(""))
  val editUrl = controllers.routes.Repositories.edit(slug=slug.getOrElse(""))
  val deleteUrl = controllers.routes.Repositories.confirmDelete(slug=slug.getOrElse(""))

  override def getSubordinateItems = Map(
    // FIXME: Find a better way of determining if
    // a contact is 'empty' and should be deleted
    "addressOf" -> contact.filterNot(c => c.streetAddress.isEmpty && c.city.isEmpty).map { c =>
      Map(
        "index_name" -> Contact.indexName,
        "data" -> c.toMap)
    }
  )          
  override def getIncomingSubordinateRelations = List("addressOf")

  def toMap = {
    Map("slug" -> slug) ++
    identity.toMap ++
    description.toMap ++
    access.toMap ++
    services.toMap ++
    control.toMap ++
    admin.toMap
  }

  def withSlug(slug: String) = copy(slug=Some(slug))
  def withContacts(contacts: List[Contact]) = copy(contact=contacts)
}

case class RepositoryIdentity(
  val identifier: String = "",
  val name: String = "",
  val parallelFormsOfName: List[String] = Nil,
  val otherFormsOfName: List[String] = Nil,
  val typeOfEntity: Option[Int] = None 
) {
  def otherNames = Nil
  def toMap = Map(
    "identifier" -> identifier,
    "name" -> name,
    "parallel_forms_of_name" -> parallelFormsOfName.filterNot(_.isEmpty).mkString(",,"),
    "other_forms_of_name" -> otherFormsOfName.filterNot(_.isEmpty).mkString(",,"),
    "type_of_entity" -> typeOfEntity
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

case class RepositoryAdmin(
  val publicationStatus: Int = 0
) {
  def toMap = Map(
    "publication_status" -> publicationStatus
  )
}

