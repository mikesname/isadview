package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import models.{Repository,Contact,Collection,FuzzyDate,Authority => AuthFile}  // clashes with 'Authority' in Auth trait
import forms.AuthorityForm


object Authorities extends Controller with Auth with Authorizer with ControllerHelpers {
  def detail(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { auth =>
        Async {
          // get collections
          Collection.findRelatedTo(auth, Collection.Direction.In, "createdBy").map { createdCollections =>
            Async {
              Collection.findRelatedTo(auth, Collection.Direction.In, "mentionedIn").map { mentionedCollections =>
                Ok(views.html.authority.detail(auth, auth.description, createdCollections, mentionedCollections))
              }
            }
          }
        }
      }
    }
  }

  def new_ = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.authority.form(f=AuthorityForm.form, action=routes.Authorities.create))
  }

  def create = optionalUserAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    AuthorityForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.authority.form(f=errorForm, action=routes.Authorities.create))
      },
      data => {
        Async {
          AuthFile.create(new AuthFile(description=data)).map { created =>
            Redirect(routes.Authorities.detail(slug=created.slug.get))
          }
        }
      }
    )
  }

  def edit(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val form = AuthorityForm.form.fill(authority.description)
        val action = routes.Authorities.save(slug)
        Ok(views.html.authority.form(f=form, action=action, r=Some(authority)))
      }
    }
  }

  def save(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        AuthorityForm.form.bindFromRequest(formData).fold(
          errorForm => {
            BadRequest(
            views.html.authority.form(f=errorForm,
            action=routes.Authorities.save(slug), r=Some(authority)))
          },
          data => {
            Async {
              val newdata = authority.copy(description=data)
              AuthFile.persist(authority.id, newdata).map { updated =>
                Redirect(routes.Authorities.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }

  def confirmDelete(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val action = routes.Authorities.delete(slug)
        Ok(views.html.basedelete(c=authority, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        AuthFile.delete(authority.id, authority)
        Redirect(routes.Search.list("authority"))
      }
    }
  }
}
