package models

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import neo4j.data._

object UserProfile extends Neo4jDataSource[UserProfile] {
  val indexName = "userprofile"

  def apply(data: net.liftweb.json.JsonAST.JValue): UserProfile = {
    new UserProfile(
      id = idFromUrl((data \ "self").extractOpt[String]),
      userId = (data \ "data" \ "user_id").extractOpt[Long].getOrElse(-1L),
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      data = ProfileData(
        name = (data \ "data" \ "name").extractOpt[String],
        about = (data \ "data" \ "about").extractOpt[String],
        location = (data \ "data" \ "location").extractOpt[String],
        website = (data \ "data" \ "website").extractOpt[String],
        languages = (data \ "data" \ "languages").extractOpt[String].getOrElse("").split(",").toList.filterNot(_.isEmpty)
      )
    )
  }

  def apply(userId: Long) = new UserProfile(
      -1, userId, None, None, new ProfileData(None, None, None, None, Nil))
}


case class UserProfile(
  val id: Long = -1,
  val userId: Long = -1,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val data: ProfileData
) extends Neo4jModel {
  def name = data.name.getOrElse("")
  //val detailUrl = controllers.routes.Users.detail(slug=auth.username.getOrElse(""))
  //val editUrl =  controllers.routes.Users.edit(slug=auth.username.getOrElse(""))
  //val deleteUrl = controllers.routes.Users.confirmDelete(slug=auth.username.getOrElse(""))

  def toMap = {
    Map(
      "user_id" -> userId,
      "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
      "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
    ) ++ data.toMap
  }
}

case class ProfileData(
    val name: Option[String] = None,
    val about: Option[String] = None,
    val location: Option[String] = None,
    val website: Option[String] = None,
    val languages: List[String] = Nil
) {
  def toMap = Map(
    "name" -> name,
    "about" -> about,
    "location" -> location,
    "website" -> website,
    "languages" -> languages.filterNot(_.isEmpty).mkString(",")
  )
}

