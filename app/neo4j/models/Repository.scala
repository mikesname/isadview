package neo4j.models


object Repository extends JsonInstantiatable[Repository] {
  implicit val formats = net.liftweb.json.DefaultFormats
  
  def fromJson(data: net.liftweb.json.JsonAST.JValue) = {
    Repository(
      url = (data \ "self").extractOpt[String],
      identity = RepositoryIdentity(
        identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
        name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        slug = (data \ "data" \ "slug").extractOpt[String].getOrElse("")
      ),
      description = RepositoryDescription(
        geoculturalContext = (data \ "data" \ "geocultural_context").extractOpt[String],
        history = (data \ "data" \ "history").extractOpt[String]
      ),
      admin = RepositoryAdmin(
        publicationStatus = (data \ "data" \"publication_status").extractOpt[Int].getOrElse(0)
      )
    )
  }
}




case class Repository(
  val identity: RepositoryIdentity,
  val description: RepositoryDescription,
  val admin: RepositoryAdmin,
  val url: Option[String] = None
) extends Description {
  def toMap = {
    identity.toMap ++
    description.toMap ++
    admin.toMap
  }
}

case class RepositoryIdentity(
  val identifier: String = "",
  val slug: String = "",
  val name: String = ""
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
  val history: Option[String] = None
) {
  def toMap = Map(
    "geocultural_context" -> geoculturalContext,
    "history" -> history
  )
}

case class RepositoryAdmin(
  val publicationStatus: Int = 0
) {
  def toMap = Map(
    "publication_status" -> publicationStatus
  )
}

