package models

import neo4j.data._
import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat


object VirtualCollection extends Neo4jDataSource[VirtualCollection] {
  val indexName = "virtualcollection"

  def apply(data: net.liftweb.json.JsonAST.JValue): VirtualCollection = {
    VirtualCollection(
      id = idFromUrl((data \ "self").extractOpt[String]),
      slug = (data \ "data" \ "slug").extractOpt[String],
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      description = VirtualCollectionDescription(
        name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
        description = (data \ "data" \ "description").extractOpt[String]
      )
    )
  }
}

case class VirtualCollection(
  val id: Long = -1,
  val slug: Option[String] = None,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val description: VirtualCollectionDescription
) extends Neo4jSlugModel {
  def name = description.name
  def toMap = Map(
    "element_type" -> VirtualCollection.indexName,
    "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
    "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
  ) ++ description.toMap
  def withSlug(slug: String) = copy(slug=Some(slug))
}

case class VirtualCollectionDescription(
  val name: String,
  val description: Option[String]
) {
  def toMap = Map(
    "name" -> name,
    "description" -> description
  )
}


