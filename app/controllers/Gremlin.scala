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

  /*
   * Play Forms don't currently support multi-value select widgets. We 
   * need to transform the input from:
   *  key -> Seq(va1, val2, val3) to:
   *  key[0] -> Seq(val1), key[1] -> Seq(val2), key[2] -> Seq(val3)
   */
  def transformMultiSelects(formData: Option[Map[String,Seq[String]]], multies: List[String]) = {
    formData.map(b => {
      b.flatMap { (t: (String,Seq[String])) =>
        t match {
          case (n, s) if multies.contains(n) => {
            s.zipWithIndex.map(t => n + "[" + t._2 + "]" -> List(t._1))
          }
          case other => List(other)
        }
      }
    }).getOrElse(Map[String,Seq[String]]())
  }

  def repositoryDetail(slug: String) = Action { implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repo =>
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
      Collection.fetchBySlug(slug).map { collection =>
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
      Authority.fetchBySlug(slug).map { auth =>
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
      Collection.fetchBySlug(slug).map { collection =>
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
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "conditions.languages",
      "conditions.scripts",
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    Async {
      Collection.fetchBySlug(slug).map { collection =>
        CollectionForm.form.bindFromRequest(formData).fold(
          errorForm => {
            BadRequest(
            views.html.collection.form(f=errorForm,
            action=routes.Gremlin.collectionSave(slug), c=Some(collection)))
          },
          data => {
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
              gremlin("update_indexed_vertex_with_subordinates", params).map { resp =>
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
