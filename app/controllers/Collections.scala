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



object Collections extends Controller with ControllerHelpers {
  def detail(slug: String) = Action { implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Async {
          // get dates
          FuzzyDate.findRelatedTo(collection, FuzzyDate.Direction.In, "locatesInTime").map { dates =>
            Async {
              Repository.findRelatedTo(collection, Repository.Direction.Out, "heldBy").map { repos =>
                val repo = repos.head
                Async {
                  Authority.findRelatedTo(collection, Authority.Direction.Out, "createdBy").map { auths =>
                    val creator = auths.headOption
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

  def edit(slug: String) = Action { implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Async {
          // get dates
          FuzzyDate.findRelatedTo(collection, FuzzyDate.Direction.In, "locatesInTime").map { dates =>
            val form = CollectionForm.form.fill(collection.withDates(dates))
            val action = routes.Collections.save(slug)
            Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
          }
        }
      }
    }
  }

  def save(slug: String) = Action { implicit request =>
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
            action=routes.Collections.save(slug), c=Some(collection)))
          },
          data => {
            Async {
              Collection.persist(collection.id, data.withSlug(slug)).map { updated =>
                Redirect(routes.Collections.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }
  
  def confirmDelete(slug: String) = Action { implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val action = routes.Collections.delete(slug)
        Ok(views.html.basedelete(c=collection, action=action))
      }
    }
  }

  def delete(slug: String) = Action { implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Collection.delete(collection.id, collection)
        Redirect(routes.Search.list("collection"))
      }
    }
  }
}