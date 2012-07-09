package models

import neo4j.data._
import java.util.Date
import play.api.libs.concurrent.Promise
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

  //override def fetchByFieldOption(field: String, value: String): Promise[Option[VirtualCollection]] = {
  //  val params = Map(
  //    "index_name" -> indexName,
  //    "key" -> field,
  //    "query_string" -> value,
  //    "inRels" -> List(),
  //    "outRels" -> List("contains")
  //  )
  //  gremlin("query_exact_index_with_related", params).map(response => {
  //    val items = getJson(response).children
  //    items.headOption.map(apply(_)).map { vc =>
  //      items.tail.foldLeft(vc) { (c: VirtualCollection, json: net.liftweb.json.JsonAST.JValue) =>
  //        (json \ "data" \ TypeKey).extractOpt[String].map { eletype =>
  //          eletype match {
  //            case Collection.indexName => c.withItem(Collection(json))
  //            case Repository.indexName => c.withItem(Repository(json))
  //            case Authority.indexName => c.withItem(Authority(json))
  //            case _ => c
  //          }
  //        }.getOrElse(c)
  //      }
  //    }
  //  })
  //}
}


case class ItemPointer(
  val edge: Edge,
  val item: Neo4jSlugModel
)


case class VirtualCollection(
  val id: Long = -1,
  val slug: Option[String] = None,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val items: List[ItemPointer] = Nil,
  val description: VirtualCollectionDescription
) extends Neo4jSlugModel with CrudUrls {
  def name = description.name
  def summary = description.description
  val detailUrl = controllers.routes.VirtualCollections.detail(id=id)
  val editUrl = controllers.routes.VirtualCollections.edit(id=id)
  val deleteUrl = controllers.routes.VirtualCollections.confirmDelete(id=id)


  def toMap = Map(
    VirtualCollection.TypeKey -> VirtualCollection.indexName,
    "slug" -> slug,
    "created_on" -> createdOn.map(ISODateTimeFormat.dateTime.print(_)),
    "updated_on" -> updatedOn.map(ISODateTimeFormat.dateTime.print(_))
  ) ++ description.toMap
  def withSlug(slug: String) = copy(slug=Some(slug))
  def withItem(item: ItemPointer) = copy(items = items ++ List(item))
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


