package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._

import neo4j.models.{Repository,Contact}


case class NoResultsFound(err: String = "") extends Exception
case class MultipleResultsFound(err: String = "") extends Exception


class GremlinResponse(r: Response) {

  implicit val formats = net.liftweb.json.DefaultFormats
  
  def toList[T: Manifest]: Seq[T] = {
    // FIXME: The response is coming back encoding wrong, so fix it with a hack...
    net.liftweb.json.parse(new String(r.body.getBytes("ISO-8859-1"), "UTF-8")).children.map(_.extract[T])
  }

  def one[T: Manifest]: T = {
    var list = toList[T]
    if (list.length == 0) throw new NoResultsFound()
    else if (list.length > 1) throw new MultipleResultsFound()
    list.head
  }

}

object Gremlin extends Controller {

  val scripts = new neo4j.ScriptSource()
  val gremlinPath = "http://localhost:7474/db/data/ext/GremlinPlugin/graphdb/execute_script"
  
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  def gremlin(scriptName: String, params: AnyRef): Promise[Response] = {
    scripts.loadScript("app/neo4j/gremlin.groovy")
    val scriptBody = scripts.get(scriptName)
    val data = Map("script" -> scriptBody, "params" -> params)
    WS.url(gremlinPath).withHeaders(headers.toList: _*).post(generate(data))
  }

  def detail(index: String, slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> index,
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val repo = new GremlinResponse(r1).one[Repository]
        Async {
          // get contacts
          gremlin("inV", Map("_id" -> repo.id.getOrElse(0), "label" -> "addressOf")).map { r2 =>
            val contacts = new GremlinResponse(r2).toList[Contact]
            Ok(views.html.repositoryDetail(repo=repo, data=repo.data, contacts=contacts))
          }
        }
      }
    }
  }
}
