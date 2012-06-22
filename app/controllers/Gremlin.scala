package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._

import neo4j.models.{Repository,Contact,Collection,FuzzyDate,Authority}
import neo4j.forms.CollectionForm



class GremlinResponse(r: Response) {
  
}


object Gremlin extends Controller {

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

  def repositoryDetail(slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> "repository",
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val repo = Repository.one(getJson(r1))
        Async {
          // get contacts
          gremlin("inV", Map("_id" -> repo.id, "label" -> "addressOf")).map { r2 =>
            val contacts = Contact.list(getJson(r2))
            Ok(views.html.repository.detail(repo=repo, contacts=contacts))
          }
        }
      }
    }
  }

  def collectionDetail(slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> "collection",
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val collection = Collection.one(getJson(r1))
        Async {
          // get dates
          gremlin("inV", Map("_id" -> collection.id, "label" -> "locatesInTime")).map { r2 =>
            val dates = FuzzyDate.list(getJson(r2))
            Async {
              gremlin("outV", Map("_id" -> collection.id, "label" -> "heldBy")).map { r3 =>
                val repo = Repository.one(getJson(r3))
                Async {
                  gremlin("outV", Map("_id" -> collection.id, "label" -> "createdBy")).map { r4 =>
                    val creator: Option[Authority] = try {
                      Some(Authority.one(getJson(r4)))
                    } catch {
                      case e: neo4j.models.NoResultsFound => None
                      case other => throw other
                    }
                    Ok(views.html.collection.detail(collection, dates, repo, creator))
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def authorityDetail(slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> "authority",
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val auth = Authority.one(getJson(r1))
        Async {
          // get collections
          gremlin("inV", Map("_id" -> auth.id, "label" -> "createdBy")).map { r2 =>
            val createdCollections = Collection.list(getJson(r2))
            Async {
              gremlin("inV", Map("_id" -> auth.id, "label" -> "mentionedIn")).map { r3 =>
                val mentionedCollections = Collection.list(getJson(r3))
                Ok(views.html.authority.detail(auth, createdCollections, mentionedCollections))
              }
            }
          }
        }
      }
    }
  }

  def collectionEdit(slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> "collection",
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val collection = Collection.one(getJson(r1))
        Async {
          // get dates
          gremlin("inV", Map("_id" -> collection.id, "label" -> "locatesInTime")).map { r2 =>
            val dates = FuzzyDate.list(getJson(r2))
            val form = CollectionForm.form.fill(collection.withDates(dates))
            val action = routes.Gremlin.collectionSave(slug)
            Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
          }
        }
      }
    }
  }

  def collectionSave(slug: String) = Action { implicit request =>
    Async {
      val params = Map(
        "index_name" -> "collection",
        "key" -> "slug",
        "query_string" -> slug
      )
      gremlin("query_exact_index", params).map { r1 =>
        val collection = Collection.one(getJson(r1))
        CollectionForm.form.bindFromRequest.fold(
          errorForm => {
            BadRequest(
            views.html.collection.form(f=errorForm,
            action=routes.Gremlin.collectionSave(slug), c=Some(collection)))
          },
          data => {
            println("Got Data", data)
            Async {
              val params = Map(
                "index_name" -> "collection",
                "_id" -> collection.id,
                "data" -> data.toMap,
                "subs" -> Map(
                  "locatesInTime" -> data.identity.dates.filterNot(_.startDate.isEmpty).map(d => {
                    Map(
                      "index_name" -> "fuzzydate",
                      "data" -> d.toMap
                    )
                  })
                )
              )
              println("Running params: " + params)
              gremlin("update_indexed_vertex_with_subordinates", params).map { resp =>
                println(resp.body)
                // TODO: Handle error if this doesn't work!
                val updated = Collection(getJson(resp))
                Redirect(routes.Gremlin.collectionDetail(slug=updated.identity.slug))
              }
            }
          }
        )
      }
    }
  }
}
