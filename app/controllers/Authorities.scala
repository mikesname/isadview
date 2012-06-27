package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.AuthorityForm


object Authorities extends Controller with ControllerHelpers {
  def detail(slug: String) = Action { implicit request =>
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

  def edit(slug: String) = Action { implicit request =>
    Async {
      Authority.fetchBySlug(slug).map { authority =>
        val form = AuthorityForm.form.fill(authority)
        val action = routes.Authorities.save(slug)
        Ok(views.html.authority.form(f=form, action=action, r=Some(authority)))
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
      Authority.fetchBySlug(slug).map { authority =>
        AuthorityForm.form.bindFromRequest(formData).fold(
          errorForm => {
            BadRequest(
            views.html.authority.form(f=errorForm,
            action=routes.Authorities.save(slug), r=Some(authority)))
          },
          data => {
            Async {
              Authority.persist(authority.id, data.withSlug(slug)).map { updated =>
                Redirect(routes.Authorities.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }

  def confirmDelete(slug: String) = Action { implicit request =>
    Async {
      Authority.fetchBySlug(slug).map { authority =>
        val action = routes.Authorities.delete(slug)
        Ok(views.html.basedelete(c=authority, action=action))
      }
    }
  }

  def delete(slug: String) = Action { implicit request =>
    Async {
      Authority.fetchBySlug(slug).map { authority =>
        Authority.delete(authority.id, authority)
        Redirect(routes.Search.list("authority"))
      }
    }
  }
}
