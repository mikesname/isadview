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



object Gremlin extends Controller with ControllerHelpers {

  def repositoryDetail(slug: String) = Action { implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repo =>
        Async {
          // get contacts
          Contact.findRelatedTo(repo, Contact.Direction.In, "addressOf").map { contacts =>
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

  def authorityDetail(slug: String) = Action { implicit request =>
    Async {
      Authority.fetchBySlug(slug).map { auth =>
        Async {
          // get collections
          Collection.findRelatedTo(auth, Collection.Direction.In, "createdBy").map { createdCollections =>
            Async {
              Collection.findRelatedTo(auth, Collection.Direction.In, "mentionedIn").map { mentionedCollections =>
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
          FuzzyDate.findRelatedTo(collection, FuzzyDate.Direction.In, "locatesInTime").map { dates =>
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
              Collection.persist(collection.id, data).map { updated =>
                Redirect(routes.Gremlin.collectionDetail(slug=updated.identity.slug))
              }
            }
          }
        )
      }
    }
  }
}
