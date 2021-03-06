package neo4j.data

import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.concurrent.Promise

import neo4j.json.{JsonBuilder,GremlinError}
import play.api.PlayException
import play.api.Play.current
import com.codahale.jerkson.Json._

import neo4j.query._
import neo4j.GremlinHelper

case class NoResultsFound(err: String = "") extends PlayException("NoResultsFound", err)
case class MultipleResultsFound(err: String = "") extends PlayException("MultipleResultsFound", err)


trait Neo4jModel {
  val id: Long
  def toMap: Map[String,Any]
  def getSubordinateItems: Map[String,List[Map[String,Any]]] = Map()
  def getIncomingSubordinateRelations: List[String] = Nil
  def getOutgoingSubordinateRelations: List[String] = Nil

  def formatDate(d: org.joda.time.DateTime) = ISODateTimeFormat.dateTime.print(d)
  def formatSolrDate(d: org.joda.time.DateTime) = ISODateTimeFormat.dateTime.withZone(DateTimeZone.UTC).print(d)
}

trait Neo4jSlugModel extends Neo4jModel with GremlinHelper {
  self: Neo4jModel with models.CrudUrls =>
  def name: String
  def withSlug(slug: String): Neo4jModel
  def summary: Option[String]
  def details: Map[String, Any] = Map()

  // FIXME: These are only here because we need to somehow combine
  // SlugModel and the CrudUrls model, and the self type isn't working
  def detailUrl: play.api.mvc.Call
  def editUrl: play.api.mvc.Call
  def deleteUrl: play.api.mvc.Call
}


trait IndexedEntity {
  /*
   * Implementing objects must specify this as the TypeKey
   * and the name of vertex indexes.
   */
  val indexName: String
  def initialize(): Promise[Boolean]
}

trait IndexedVertex extends IndexedEntity {
  self: GremlinHelper =>
  // Create an index on the server at startup
  def initialize(): Promise[Boolean] = {
    val params = Map("index_name" -> indexName, "index_params" -> None)
    gremlin("get_or_create_vertex_index", params).map { resp =>
      true
    }
  }
}

trait IndexedEdge extends IndexedEntity {
  self: GremlinHelper =>
  // Create an index on the server at startup
  def initialize(): Promise[Boolean] = {
    val params = Map("index_name" -> indexName, "index_params" -> None)
    gremlin("get_or_create_edge_index", params).map { resp =>
      true
    }
  }
}

trait Neo4jRelationship extends IndexedEdge with GremlinHelper


trait Neo4jDataSource[T] extends JsonBuilder[T] with IndexedVertex with GremlinHelper {
  /*
   * The name of the (mandatory) neo4j property that marks
   * denotes the type of a node.
   */
  val TypeKey = "element_type"


  /**
   *  The headers that get sent to the Neo4j Gremlin plugin for a
   *  JSON request/response.
   */
  def nowDateTime = ISODateTimeFormat.dateTime.print(DateTime.now)

  def create0(item: Neo4jModel): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "data" -> (item.toMap + ("created_on" -> nowDateTime)),
      "subs" -> item.getSubordinateItems
    )
    // FIXME: Make this asyncronous somehow by composing this Promise and
    // the parent one.
    gremlin("create_indexed_vertex_with_subordinates", params).map { resp =>
      apply(getJson(resp))
    }
  }

  def create(item: Neo4jSlugModel): Promise[T] = {
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

  def persist(nodeId: Long, item: Neo4jModel): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "_id" -> nodeId,
      // HACK: Add updated_on key to data items
      "data" -> (item.toMap + ("updated_on" -> nowDateTime)),
      "subs" -> item.getSubordinateItems
    )
    gremlin("update_indexed_vertex_with_subordinates", params).map { resp =>
      // TODO: Handle error if this doesn't work!
      apply(getJson(resp))
    }
  }
  
  def delete(nodeId: Long): Promise[Boolean] = {
    gremlin("delete_vertex", Map("_id" -> nodeId)).map(response => {
      true
    })
  }

  def delete(nodeId: Long, item: Neo4jModel): Promise[Boolean] = {
    val params = Map(
      "_id" -> nodeId,
      "inRels" -> item.getIncomingSubordinateRelations,
      "outRels" -> item.getOutgoingSubordinateRelations
    )
    gremlin("delete_vertex_with_related", params).map(response => {
      true
    })
  }

  def deleteRelationship(rel: Long): Promise[Boolean] = {
    gremlin("delete_edge", Map("_id" -> rel)).map(response => {
      true
    });
  }

  def createRelationship(from: Neo4jModel, to: Neo4jModel, label: String): Promise[net.liftweb.json.JsonAST.JValue] = {
    createRelationship(from.id, to.id, label)
  }

  def createRelationship(from: Long, to: Long, label: String, data: Map[String,Any] = Map()) = {
    val params = Map(
      "outV" -> from,
      "label" -> label,
      "inV" -> to,
      "data" -> (data + ("created_on" -> nowDateTime)),
      "index_name" -> label,
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

  def findRelatedTo(other: Neo4jModel, direction: Direction.Direction, label: String): Promise[List[T]] = {
    gremlin(direction.toString, Map("_id" -> other.id, "label" -> label)).map { response =>
      list(getJson(response))
    }
  }

  def query = Query(apply, indexName)

}

