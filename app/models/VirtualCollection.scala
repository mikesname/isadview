package models

import java.util.Date
import play.api.libs.concurrent.Promise
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat


object VirtualCollection extends neo4j.DataSource[VirtualCollection] {
  val indexName = "virtualcollection"

  case object Contains extends neo4j.Relationship {
    val indexName = "contains"
  }

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


case class ItemPointer(
  val edge: Edge,
  val item: Description
)


case class VirtualCollection(
  val id: Long = -1,
  val slug: Option[String] = None,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val items: List[ItemPointer] = Nil,
  val description: VirtualCollectionDescription
) extends neo4j.SlugModel with Entity with CrudUrls with CreatedUpdated {
  def name = description.name
  def summary = description.description
  val detailUrl = controllers.routes.VirtualCollections.detail(id=id)
  val editUrl = controllers.routes.VirtualCollections.update(id=id)
  val deleteUrl = controllers.routes.VirtualCollections.delete(id=id)

  def lastUpdated = if (updatedOn.isDefined) updatedOn else createdOn

  def toMap = Map(
    VirtualCollection.TypeKey -> VirtualCollection.indexName,
    "slug" -> slug,
    "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
    "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
  ) ++ description.toMap
  def withSlug(slug: String) = copy(slug=Some(slug))
  def withItem(item: ItemPointer) = copy(items = items ++ List(item))

  private lazy val itemIds: List[Long] = items.map(_.item.id)
  def hasItem(id: Long): Boolean = itemIds.contains(id)
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


