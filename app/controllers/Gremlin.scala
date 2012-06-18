package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS

import net.liftweb.json

import com.codahale.jerkson.Json._

object Gremlin extends Controller {

  implicit val formats = json.DefaultFormats

  val gremlinPath = "http://localhost:7474/db/data/ext/GremlinPlugin/graphdb/execute_script"
  val testScript = Map(
    "script" -> "g.idx('repository')[[\"$key\":value]]",
    "params" -> Map(
      "key" -> "slug",
      "value" -> "wiener-library"
    )
  )

  def headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )

  def gremlin(script: AnyRef) = {
    WS.url(gremlinPath).withHeaders(headers.toList: _*).post(generate(script))
  }

  case class Repo(
    val self: String = "",
    val data: RepoData
  )
  case class RepoData(
    val slug: String = "",
    val name: String = "",
    val identifier: String = "",
    val publication_status: Int = 0,
    val date: Option[java.util.Date] = None
  )

  def async = Action {
    Async {

      val extractEid = """.+/(\d+)$""".r

      gremlin(testScript).map { response =>
        println("%s, %s".format(response, response.status))
        val parsed = json.parse(response.body)
        val repos = parsed.children.map(_.extract[Repo])
        println("Repos: %s".format(repos))
        //implicit val formats = Serialization.formats(NoTypeHints)
        repos.map( r =>
            println("Repo: %s => %s".format(r, json.Serialization.write(r)))

        )
        Ok(response.json)
      }
    }
  }
}
