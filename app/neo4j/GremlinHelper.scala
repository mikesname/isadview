package neo4j

import play.api.libs.ws.{WS,Response}
import com.codahale.jerkson.Json._
import play.api.libs.concurrent.Promise
import play.api.Play.current
import neo4j.json.{JsonBuilder,GremlinError}

import play.api.libs.concurrent.execution.defaultContext

/*
 * Trait containing methods related to posting and decoding 
 * Gremlin script/responses
 */
trait GremlinHelper {
  lazy val scripts = new neo4j.ScriptSource()
  lazy val gremlinPath = play.api.Play.configuration.getString("gremlin").getOrElse(
      sys.error("The path to the Neo4j Gremlin plugin is not specified in application.conf"))

  /**
   *  For as-yet-undetermined reasons that data coming back from Neo4j seems
   *  to be encoded as ISO-8859-1, so we need to convert it to UTF-8. Obvs.
   *  this problem should eventually be fixed at the source, rather than here.
   */
  def fixEncoding(s: String) = new String(s.getBytes("ISO-8859-1"), "UTF-8")

  /*
   * Enum for declaring direction of relationships.
   */
  object Direction extends Enumeration("inV", "outV") {
    type Direction = Value
    val In, Out = Value
  }
  
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  /**
   *  Only Lift's JSON decoder seems able to parse JSON without
   * knowing it's exact structure in advance. If we get a Gremlin
   * error raise an exception and try and report it sensibly.
   */
  implicit val formats = net.liftweb.json.DefaultFormats
  def getJson(r: Response): net.liftweb.json.JsonAST.JValue = {
    try {
      val data = net.liftweb.json.parse(fixEncoding(r.body))
      data.extractOpt[GremlinError].map { error =>
        println(r.body)
        throw error
      }.getOrElse(data)
    } catch {
      // FIXME: Make this more sensible... unfortunately the response status
      // doesn't help us when it's a Gremlin script error causing an unexpected
      // response payload
      case e: net.liftweb.json.JsonParser.ParseException => throw new Exception("JSON error parsing: " + r.body)
      case other => throw other
    }
  }
  import play.api.libs.concurrent.execution.defaultContext
  def gremlin(scriptName: String, params: AnyRef): Promise[Response] = {
    println("Dispatching with: " + params)
    scripts.loadScript("groovy/gremlin.groovy")
    val scriptBody = scripts.get(scriptName)
    val data = Map("script" -> scriptBody, "params" -> params)
    WS.url(gremlinPath).withHeaders(headers.toList: _*).post(generate(data))
  }
}

object GremlinHelper extends GremlinHelper

// vim: set ts=4 sw=4 et:
