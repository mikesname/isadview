package neo4j

import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.ISODateTimeFormat

import neo4j.json.{JsonBuilder,GremlinError}
import play.api.PlayException
import play.api.Play.current
import com.codahale.jerkson.Json._
import play.api.libs.concurrent.execution.defaultContext
import play.api.libs.concurrent.Promise

import neo4j.query._

case class NoResultsFound(err: String = "") extends PlayException("NoResultsFound", err)
case class MultipleResultsFound(err: String = "") extends PlayException("MultipleResultsFound", err)


trait Model extends GremlinHelper {
  val id: Long
  def toMap: Map[String,Any]
  def getSubordinateItems: Map[String,List[Map[String,Any]]] = Map()
  def getIncomingSubordinateRelations: List[String] = Nil
  def getOutgoingSubordinateRelations: List[String] = Nil
}

/*
 * Trait able to auto-generate it's slug from
 * a name field
 */
trait SlugModel extends Model {
  def slug: Option[String]
  def name: String
  def withSlug(slug: String): SlugModel
}


trait Relationship extends IndexedEdge with GremlinHelper


trait DataSource[T <: Model] extends JsonBuilder[T] with IndexedVertex with GremlinHelper {

  object Callbacks extends Enumeration("create", "update", "delete") {
    type Callback = Value
    val create, update, delete = Value
  }

  // Callback functions take a T and don't return anything.
  type CbFunc = T => Unit

  private var listeners = scala.collection.mutable.Map[Callbacks.Callback, List[CbFunc]]()

  def addListener(cb: Callbacks.Callback)(f: CbFunc) = listeners.get(cb) match {
    case None => listeners(cb) = List(f)
    case Some(list) => listeners(cb) = (list ++ List(f)).distinct // ensure no duplicates
  }

  def addListeners(cbs: Callbacks.Callback*)(f: CbFunc) = 
    cbs.map(addListener(_)(f))

  private def callCallbacks(cb: Callbacks.Callback, item: T) = {
    for (f <- listeners.getOrElse(cb, Nil))
      f(item)
  }

  private def withCallback(cb: Callbacks.Callback)(item: T) = {
    callCallbacks(cb, item)
    item
  }
  
  /**
   *  The headers that get sent to the Neo4j Gremlin plugin for a
   *  JSON request/response.
   */
  def nowDateTime = ISODateTimeFormat.dateTime.print(DateTime.now)

  def create0(item: Model): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "data" -> (item.toMap + ("created_on" -> nowDateTime)),
      "subs" -> item.getSubordinateItems
    )
    // FIXME: Make this asyncronous somehow by composing this Promise and
    // the parent one.
    gremlin("create_indexed_vertex_with_subordinates", params).map { resp =>
      withCallback(Callbacks.create) {
        apply(getJson(resp))
      }
    }
  }

  def create(item: SlugModel): Promise[T] = {
    val initial = app.util.Helpers.slugify(item.name)
    val slugparams = Map("index_name" -> indexName, "key" -> "slug", "initial" -> initial)
    // NB: Note the use of flatMap here, because create0 returns another
    // Promise and we want to compose them together.
    // TODO: Sanity check slug response...
    gremlin("ensure_unique_for_index", slugparams).flatMap { slugresp =>
      println(slugresp.body)
      var slug: String = parse(slugresp.body)
      create0(item.withSlug(slug))
    }
  }

  def persist(nodeId: Long, item: T): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "_id" -> nodeId,
      // HACK: Add updated_on key to data items
      "data" -> (item.toMap + ("updated_on" -> nowDateTime)),
      "subs" -> item.getSubordinateItems
    )
    gremlin("update_indexed_vertex_with_subordinates", params).map { resp =>
      // TODO: Handle error if this doesn't work!
      withCallback(Callbacks.update) {
        apply(getJson(resp))
      }
    }
  }
  
  def delete(nodeId: Long, item: T): Promise[Boolean] = {
    val params = Map(
      "_id" -> nodeId,
      "inRels" -> item.getIncomingSubordinateRelations,
      "outRels" -> item.getOutgoingSubordinateRelations
    )
    gremlin("delete_vertex_with_related", params).map(response => {
      withCallback(Callbacks.delete)(item)
      true
    })
  }

  def deleteRelationship(rel: Long): Promise[Boolean] = {
    gremlin("delete_edge", Map("_id" -> rel)).map(response => {
      true
    });
  }

  def createRelationship(from: Model, to: Model, rel: Relationship): Promise[net.liftweb.json.JsonAST.JValue] = {
    createRelationship(from.id, to.id, rel)
  }

  def createRelationship(from: Long, to: Long, rel: Relationship, data: Map[String,Any] = Map()) = {
    val params = Map(
      "outV" -> from,
      "label" -> rel.indexName,
      "inV" -> to,
      "data" -> (data + ("created_on" -> nowDateTime)),
      "index_name" -> rel.indexName,
      "keys" -> null,
      "label_var" -> "label"
    )
    gremlin("create_indexed_edge", params).map { resp =>
      // TODO: Return some kind of Edge model
      getJson(resp)
    }
  }

  def fetchByID(id: Long): Promise[T] = {
    gremlin("v", Map("_id" -> id)).map(response => apply(getJson(response)))
  }

  def fetchByField(field: String, value: String): Promise[T] = {
    fetchByFieldOption(field, value).map {
      case Some(itemOpt) => itemOpt
      case None => throw NoResultsFound("for field: '%s' and query '%s'".format(field, value))
    }
  }

  def fetchByFieldOption(field: String, value: String): Promise[Option[T]] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> field,
      "query_string" -> value
    )
    gremlin("query_exact_index", params).map(response => {
      val item = list(getJson(response))
      if (item.length > 1)
        throw new MultipleResultsFound("for field: '%s' and query '%s'".format(field, value))
      item.headOption
    })
  }

  /*
   * Shortcut for finding items by slug
   */
  def fetchBySlug(slug: String): Promise[T] = {
    fetchByField(field="slug", value=slug)
  }

  def findRelatedTo(other: Model, direction: Direction.Direction, rel: Relationship): Promise[List[T]] = {
    gremlin(direction.toString, Map("_id" -> other.id, "label" -> rel.indexName)).map { response =>
      list(getJson(response))
    }
  }

  def query = Query(apply, indexName)

}

