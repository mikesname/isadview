package neo4j.models

import play.api.libs.concurrent.Promise
import play.api.libs.ws.{WS,Response}
import com.codahale.jerkson.Json._

import neo4j.json.JsonBuilder

case class NoResultsFound(err: String = "") extends Exception
case class MultipleResultsFound(err: String = "") extends Exception


trait Neo4jDataSource[T] extends JsonBuilder[T] {
  val scripts = new neo4j.ScriptSource()
  val gremlinPath = "http://localhost:7474/db/data/ext/GremlinPlugin/graphdb/execute_script"

  object Direction extends Enumeration("inV", "outV") {
    type Direction = Value
    val In, Out = Value
  }
  
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  implicit val formats = net.liftweb.json.DefaultFormats
  def getJson(r: Response) = net.liftweb.json.parse(new String(
      r.body.getBytes("ISO-8859-1"), "UTF-8"))

  def gremlin(scriptName: String, params: AnyRef): Promise[Response] = {
    scripts.loadScript("app/neo4j/gremlin.groovy")
    val scriptBody = scripts.get(scriptName)
    val data = Map("script" -> scriptBody, "params" -> params)
    WS.url(gremlinPath).withHeaders(headers.toList: _*).post(generate(data))
  }

  val indexName: String
  //def toMap: Map[String,Any]
  //abstract def subordinateModels: Map[String,List[Neo4jDataSource]]


  def persist(nodeId: Long, item: Description): Promise[T] = {
    val params = Map(
      "index_name" -> "collection",
      "_id" -> nodeId,
      "data" -> item.toMap,
      "subs" -> item.getSubordinateItems
    )
    gremlin("update_indexed_vertex_with_subordinates", params).map { resp =>
      // TODO: Handle error if this doesn't work!
      apply(getJson(resp))
    }
  }


  def fetchBySlug(slug: String): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> "slug",
      "query_string" -> slug
    )
    gremlin("query_exact_index", params).map(response => one(getJson(response)))
  }

  def findRelatedTo(other: Description, direction: Direction.Direction, label: String): Promise[List[T]] = {
    gremlin(direction.toString, Map("_id" -> other.id, "label" -> label)).map { response =>
      list(getJson(response))
    }
  }
}

