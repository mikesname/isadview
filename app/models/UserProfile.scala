package models

import org.joda.time.DateTime
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext
import org.joda.time.format.ISODateTimeFormat
import net.liftweb.json.JsonAST.{JObject,JValue}
import net.liftweb.json.Serialization.write

object UserProfile extends neo4j.DataSource[UserProfile] {
  val indexName = "userprofile"

  def apply(data: net.liftweb.json.JsonAST.JValue): UserProfile = {
    new UserProfile(
      id = idFromUrl((data \ "self").extractOpt[String]),
      userId = (data \ "data" \ "user_id").extract[Long],
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
      -1, userId, None, None, Nil, new ProfileData(None, None, None, None, Nil))

  def createVirtualCollection(profile: UserProfile, vcdesc: VirtualCollectionDescription) = {
    VirtualCollection.create(new VirtualCollection(description=vcdesc)).flatMap { created =>
      createRelationship(profile, created, "hasCollection").map { edge =>
        created
      }
    }
  }
  
  override def fetchByFieldOption(field: String, value: String): Promise[Option[UserProfile]] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> field,
      "query_string" -> value,
      "inRels" -> List(),
      "outRels" -> List("hasCollection")
    )
    gremlin("query_exact_index_with_related", params).map(response => {
      val items = getJson(response).children
      items.headOption.map(apply(_)).map { prof =>
        items.tail.foldLeft(prof) { (p: UserProfile, json: JValue) =>
          (json \ "data" \ TypeKey).extractOpt[String].map { eletype =>
            eletype match {
              case VirtualCollection.indexName => p.withCollection(VirtualCollection(json))
              case _ => p
            }
          }.getOrElse(p)
        }
      }
    })
  }

  def fetchByUserID(id: Long): Promise[Option[UserProfile]] = {
    val params = Map("user_id" -> id)

    gremlin("get_user_profile_data", params).map(response => {
      val data = getJson(response)
      val userprofile = apply((data \ "item"))
      if (userprofile.id != -1) {
        val vcs = (data \ "virtualcollections" \ "data").children.map { vcjson =>
          val vc = VirtualCollection(vcjson \ "item")
          (vcjson \\ "collections" \ "data").children.foldLeft(vc) { (vc: VirtualCollection, cjson: JValue) =>
            cjson.extract[List[JObject]] match {
              case edge :: node :: Nil => {
                (node \ "data" \ "element_type").extractOpt[String].map { eletype =>
                  eletype match {
                    case Collection.indexName => vc.withItem(new ItemPointer(edge=Edge(edge), item=Collection(node)))
                    case Repository.indexName => vc.withItem(new ItemPointer(edge=Edge(edge), item=Repository(node)))
                    case Authority.indexName => vc.withItem(new ItemPointer(edge=Edge(edge), item=Authority(node)))
                    case _ => throw sys.error("Unexpected element type in virtual collection contents: " + eletype)
                  }
                }.getOrElse(vc)
              }
              case _ => vc
            }
          }
        }
        Some(userprofile.withCollections(vcs))
      } else None
    })
  }
}


case class UserProfile(
  val id: Long = -1,
  val userId: Long = -1,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val virtualCollections: List[VirtualCollection] = Nil,
  val data: ProfileData
) extends neo4j.Model {
  def name = data.name.getOrElse("")
  //val detailUrl = controllers.routes.Users.detail(slug=auth.username.getOrElse(""))
  //val editUrl =  controllers.routes.Users.edit(slug=auth.username.getOrElse(""))
  //val deleteUrl = controllers.routes.Users.confirmDelete(slug=auth.username.getOrElse(""))

  def toMap = {
    Map(
      "user_id" -> userId,
      UserProfile.TypeKey -> UserProfile.indexName,
      "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
      "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
    ) ++ data.toMap
  }

  def withCollection(vc: VirtualCollection) = 
      copy(virtualCollections=virtualCollections ++ List(vc))
  def withCollections(vclist: List[VirtualCollection]) = copy(virtualCollections=vclist)
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

