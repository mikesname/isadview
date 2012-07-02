package neo4j.data

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.concurrent.Promise
import play.api.libs.ws.{WS,Response}
import com.codahale.jerkson.Json._

import neo4j.json.{JsonBuilder,GremlinError}
import play.api.PlayException
import play.api.Play.current

case class NoResultsFound(err: String = "") extends PlayException("NoResultsFound", err)
case class MultipleResultsFound(err: String = "") extends PlayException("MultipleResultsFound", err)


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
  val scripts = new neo4j.ScriptSource()
  val gremlinPath = play.api.Play.configuration.getString("gremlin").getOrElse(
      sys.error("The path to the Neo4j Gremlin plugin is not specified in application.conf"))

  /*
   * The name of the (mandatory) neo4j property that marks
   * denotes the type of a node.
   */
  val TypeKey = "element_type"

  /*
   * Implementing objects must specify this as the TypeKey
   * and the name of vertex indexes.
   */
  val indexName: String

  /*
   * Enum for declaring direction of relationships.
   */
  object Direction extends Enumeration("inV", "outV") {
    type Direction = Value
    val In, Out = Value
  }
  
  /**
   *  The headers that get sent to the Neo4j Gremlin plugin for a
   *  JSON request/response.
   */
  private val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  private def nowDateTime = ISODateTimeFormat.dateTime.print(DateTime.now)

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
      val data = net.liftweb.json.parse(fixEncoding(r.body))
      data.extractOpt[GremlinError].map(throw _).getOrElse(data)
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
    // TODO: Sanity check slug response...
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

  def createRelationship(from: Neo4jModel, to: Neo4jModel, label: String) = {
    val params = Map(
      "outV" -> from.id,
      "label" -> label,
      "inV" -> to.id,
      "data" -> Map(),
      "index_name" -> label,
      "keys" -> null,
      "label_var" -> "label"
    )
    gremlin("create_indexed_edge", params).map { resp =>
      // TODO: Return some kind of Edge model
      getJson(resp)
    }
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
}

