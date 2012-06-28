package neo4j.data

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.concurrent.Promise
import play.api.libs.ws.{WS,Response}
import com.codahale.jerkson.Json._

import neo4j.json.JsonBuilder

case class NoResultsFound(err: String = "") extends Exception
case class MultipleResultsFound(err: String = "") extends Exception


trait Neo4jModel {
  val id: Long
  def toMap: Map[String,Any]
  def getSubordinateItems: Map[String,List[Map[String,Any]]] = Map()
  def getIncomingSubordinateRelations: List[String] = Nil
  def getOutgoingSubordinateRelations: List[String] = Nil
}

trait Neo4jSlugModel extends Neo4jModel {
  def name: String
  def withSlug(slug: String): Neo4jModel
}


trait Neo4jDataSource[T] extends JsonBuilder[T] {
  val indexName: String
  val scripts = new neo4j.ScriptSource()
  val gremlinPath = "http://localhost:7474/db/data/ext/GremlinPlugin/graphdb/execute_script"

  object Direction extends Enumeration("inV", "outV") {
    type Direction = Value
    val In, Out = Value
  }
  
  /**
   *  The headers that get sent to the Neo4j Gremlin plugin for a
   *  JSON request/response.
   */
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  def nowDateTime = ISODateTimeFormat.dateTime.print(DateTime.now)

  /**
   *  For as-yet-undetermined reasons that data coming back from Neo4j seems
   *  to be encoded as ISO-8859-1, so we need to convert it to UTF-8. Obvs.
   *  this problem should eventually be fixed at the source, rather than here.
   */
  def fixEncoding(s: String) = new String(s.getBytes("ISO-8859-1"), "UTF-8")

  /**
   *  Only Lift's JSON decoder seems able to parse JSON without
   * knowing it's exact structure in advance. If we get a Gremlin
   * error raise an exception and try and report it sensibly.
   */
  implicit val formats = net.liftweb.json.DefaultFormats
  def getJson(r: Response) = {
    try {
      net.liftweb.json.parse(fixEncoding(r.body))
    } catch {
      // FIXME: Make this more sensible... unfortunately the response status
      // doesn't help us when it's a Gremlin script error causing an unexpected
      // response payload
      case e: net.liftweb.json.JsonParser.ParseException => throw new Exception("JSON error parsing: " + r.body)
      case other => throw other
    }
  }

  def gremlin(scriptName: String, params: AnyRef): Promise[Response] = {
    scripts.loadScript("app/neo4j/gremlin.groovy")
    val scriptBody = scripts.get(scriptName)
    val data = Map("script" -> scriptBody, "params" -> params)
    println("Updating with data: " + generate(data))
    WS.url(gremlinPath).withHeaders(headers.toList: _*).post(generate(data))
  }

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
    gremlin("ensure_unique_for_index", slugparams).flatMap { slugresp =>
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

  def fetchBySlug(slug: String): Promise[T] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> "slug",
      "query_string" -> slug
    )
    gremlin("query_exact_index", params).map(response => one(getJson(response)))
  }

  def findRelatedTo(other: Neo4jModel, direction: Direction.Direction, label: String): Promise[List[T]] = {
    gremlin(direction.toString, Map("_id" -> other.id, "label" -> label)).map { response =>
      list(getJson(response))
    }
  }
}

