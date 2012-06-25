package neo4j.models

object Authority extends Neo4jDataSource[Authority] {
  val indexName = "authority"

  def apply(data: net.liftweb.json.JsonAST.JValue): Authority = {
    Authority(
      id = idFromUrl((data \ "self").extractOpt[String]),
      identity = AuthorityIdentity(
        typeOfEntity = (data \ "data" \ "type_of_entity").extractOpt[Int].getOrElse(0),
        identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
        name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        slug = (data \ "data" \ "slug").extractOpt[String].getOrElse("")
      ),
      description = AuthorityDescription(
        datesOfExistence = (data \ "data" \ "dates_of_existence").extractOpt[String],
        history = (data \ "data" \ "history").extractOpt[String]
      ),
      admin = AuthorityAdmin(
        publicationStatus = (data \ "data" \"publication_status").extractOpt[Int].getOrElse(0)
      )
    )
  }
}


case class Authority(
  val identity: AuthorityIdentity,
  val description: AuthorityDescription,
  val admin: AuthorityAdmin,
  val id: Long = -1
) extends Description {
  def toMap = identity.toMap ++ admin.toMap
}

case class AuthorityIdentity(
  val typeOfEntity: Int = 0,
  val identifier: String = "",
  val slug: String = "",
  val name: String = ""
) {
  def otherNames = Nil
  def toMap = Map(
    "type_of_entity" -> typeOfEntity,
    "identifier" -> identifier,
    "slug" -> slug,
    "name" -> name
  )
}

case class AuthorityDescription(
  val datesOfExistence: Option[String] = None,
  val history: Option[String] = None
) {
  def toMap = Map(
    "dates_of_existence" -> datesOfExistence,
    "history" -> history
  )
}

case class AuthorityAdmin(
  publicationStatus: Int = 0
) {
  def toMap = Map("publication_status" -> publicationStatus)
}
