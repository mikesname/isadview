package models

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import neo4j.data._

object Authority extends Neo4jDataSource[Authority] {
  val indexName = "authority"

  def apply(data: net.liftweb.json.JsonAST.JValue): Authority = {
    Authority(
      id = idFromUrl((data \ "self").extractOpt[String]),
      slug = (data \ "data" \ "slug").extractOpt[String],
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      description = AuthorityDescription(
        identity = AuthorityIdentity(
          typeOfEntity = (data \ "data" \ "type_of_entity").extractOpt[Int].getOrElse(0),
          identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
          name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
          otherFormsOfName = (data \ "data" \
            "other_forms_of_name").extractOpt[String].getOrElse("").split(",,").toList.filterNot(_.isEmpty)
        ),
        description = AuthorityDetails(
          datesOfExistence = (data \ "data" \ "dates_of_existence").extractOpt[String],
          history = (data \ "data" \ "history").extractOpt[String],
          places = (data \ "data" \ "places").extractOpt[String],
          functions = (data \ "data" \ "functions").extractOpt[String],
          geneology = (data \ "data" \ "geneology").extractOpt[String],
          generalContext = (data \ "data" \ "general_context").extractOpt[String]
        ),
        // TODO: Remove this code dup: also in the Repository class
        // Probably make the sub-sections responsible for initialising themselves
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
        admin = AuthorityAdmin(
          publicationStatus = (data \ "data" \ "publication_status").extractOpt[Int].getOrElse(0)
        )
      )
    )
  }
}


case class Authority(
  val id: Long = -1,
  val slug: Option[String] = None,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val description: AuthorityDescription
) extends Neo4jSlugModel with CrudUrls {
  def name = description.identity.name
  val detailUrl = controllers.routes.Authorities.detail(slug=slug.getOrElse(""))
  val editUrl = controllers.routes.Authorities.edit(slug=slug.getOrElse(""))
  val deleteUrl = controllers.routes.Authorities.confirmDelete(slug=slug.getOrElse(""))

  def toMap = {
    Map(
      "slug" -> slug,
      "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
      "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
    ) ++ description.toMap
  }

  def withSlug(slug: String) = copy(slug=Some(slug))
}

case class AuthorityDescription(
  val identity: AuthorityIdentity,
  val description: AuthorityDetails,
  val control: AuthorityControl,
  val admin: AuthorityAdmin
) {
  def toMap = {
    identity.toMap ++
    description.toMap ++
    control.toMap ++
    admin.toMap
  }
}

case class AuthorityIdentity(
  val typeOfEntity: Int = AuthorityType.Person.id,
  val identifier: String = "",
  val name: String = "",
  val otherFormsOfName: List[String] = Nil
) {
  def otherNames = otherFormsOfName
  def toMap = Map(
    "type_of_entity" -> typeOfEntity,
    "identifier" -> identifier,
    "name" -> name,
    "other_forms_of_name" -> otherFormsOfName.filterNot(_.isEmpty).mkString(",,")
  )
}

case class AuthorityDetails(
  val datesOfExistence: Option[String] = None,
  val history: Option[String] = None,
  val places: Option[String] = None,
  val functions: Option[String] = None,
  val geneology: Option[String] = None,
  val generalContext: Option[String] = None
) {
  def toMap = Map(
    "dates_of_existence" -> datesOfExistence,
    "history" -> history,
    "places" -> places,
    "functions" -> functions,
    "geneology" -> geneology,
    "general_context" -> generalContext
  )
}

case class AuthorityControl(
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

case class AuthorityAdmin(
  publicationStatus: Int = 0
) {
  def toMap = Map("publication_status" -> publicationStatus)
}

