package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.RepositoryForm



object Repositories extends Controller with ControllerHelpers {

  def detail(slug: String) = Action { implicit request =>
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

  def edit(slug: String) = Action { implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        Async {
          // get dates
          Contact.findRelatedTo(repository, Contact.Direction.In, "addressOf").map { contacts =>
            val form = RepositoryForm.form.fill(repository.withContacts(contacts))
            val action = routes.Repositories.save(slug)
            Ok(views.html.repository.form(f=form, action=action, r=Some(repository)))
          }
        }
      }
    }
  }

  def save(slug: String) = Action { implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    Async {
      Repository.fetchBySlug(slug).map { repository =>
        RepositoryForm.form.bindFromRequest(formData).fold(
          errorForm => {
            BadRequest(
            views.html.repository.form(f=errorForm,
            action=routes.Repositories.save(slug), r=Some(repository)))
          },
          data => {
            Async {
              Repository.persist(repository.id, data.withSlug(slug)).map { updated =>
                println("UPDATED: " + updated)
                Redirect(routes.Repositories.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }
  
  def confirmDelete(slug: String) = Action { implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        val action = routes.Repositories.delete(slug)
        Ok(views.html.basedelete(c=repository, action=action))
      }
    }
  }

  def delete(slug: String) = Action { implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        Repository.delete(repository.id, repository)
        Redirect(routes.Search.list("repository"))
      }
    }
  }
}
