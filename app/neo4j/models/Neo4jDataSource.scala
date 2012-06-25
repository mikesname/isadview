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


  def fetchBySlug(slug: String): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> "slug",
      "query_string" -> slug
    )
    gremlin("query_exact_index", params).map(response => one(getJson(response)))
  }
}

