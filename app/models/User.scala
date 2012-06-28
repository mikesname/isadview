package models

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import neo4j.data._

object User extends Neo4jDataSource[User] {
  val indexName = "user"

  def apply(data: net.liftweb.json.JsonAST.JValue): User = {
    new User(
      id = idFromUrl((data \ "self").extractOpt[String]),
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      auth = UserAuth(
        username        = (data \ "data" \ "username").extractOpt[String].getOrElse(""),
        email           = (data \ "data" \ "email").extractOpt[String].getOrElse(""),
        verified        = (data \ "data" \ "verified").extractOpt[Boolean].getOrElse(false),
        salt            = (data \ "data" \ "salt").extractOpt[String].getOrElse(""),
        hashedPassword  = (data \ "data" \ "hashed_password").extractOpt[String].getOrElse("")
      ),
      profile = UserProfile(
        name = (data \ "data" \ "name").extractOpt[String],
        languages = (data \ "data" \ "languages").extractOpt[String].getOrElse("").split(",").toList.filterNot(_.isEmpty)
      )
    )
  }

  def apply(auth: UserAuth): User = {
    new User(auth=auth, profile=new UserProfile())
  }

  def authenticate(email: String, password: String): Option[User] = {
    None
  }

  def checkUniqueUsername(username: String): Boolean = {
    true
  }
}


case class User(
  val id: Long = -1,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val auth: UserAuth,
  val profile: UserProfile
) extends Neo4jModel {
  def slug = auth.username
  def name = profile.name.getOrElse(auth.username)
  //val detailUrl = controllers.routes.Users.detail(slug=auth.username.getOrElse(""))
  //val editUrl =  controllers.routes.Users.edit(slug=auth.username.getOrElse(""))
  //val deleteUrl = controllers.routes.Users.confirmDelete(slug=auth.username.getOrElse(""))

  def toMap = {
    Map(
      "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
      "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
    ) ++ profile.toMap
  }
}

case class UserAuth(
  val username: String,
  val email: String,
  val verified: Boolean,
  val salt: String,
  val hashedPassword: String
) {
  def toMap = Map(
    "username" -> username,
    "email" -> email,
    "verified" -> verified,
    "salt" -> salt,
    "hashed_password" -> hashedPassword
  )
}

case class UserProfile(
    val name: Option[String] = None,
    val languages: List[String] = Nil
) {
  def toMap = Map(
    "name" -> name,
    "languages" -> languages.filterNot(_.isEmpty).mkString(",")
  )
}

