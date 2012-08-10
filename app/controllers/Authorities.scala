package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import net.liftweb.json

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority => AuthFile}  // clashes with 'Authority' in Auth trait
import forms.AuthorityForm


object Authorities extends AuthController with ControllerHelpers {
  def detail(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { auth =>
        Async {
          // get collections
          Collection.findRelatedTo(auth, Collection.Direction.Out, AuthFile.Created).map { createdCollections =>
            Async {
              Collection.findRelatedTo(auth, Collection.Direction.Out, AuthFile.MentionedIn).map { mentionedCollections =>
                Ok(views.html.authority.detail(auth, auth.description, createdCollections, mentionedCollections))
              }
            }
          }
        }
      }
    }
  }

  def create = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Ok(views.html.authority.form(f=AuthorityForm.form, action=routes.Authorities.createPost))
  }

  def createPost = optionalUserProfileAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    AuthorityForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.authority.form(f=errorForm, action=routes.Authorities.createPost))
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

  def update(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val form = AuthorityForm.form.fill(authority.description)
        val action = routes.Authorities.updatePost(slug)
        Ok(views.html.authority.form(f=form, action=action, r=Some(authority)))
      }
    }
  }

  def updatePost(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
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
            action=routes.Authorities.updatePost(slug), r=Some(authority)))
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

  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val action = routes.Authorities.deletePost(slug)
        Ok(views.html.basedelete(c=authority, action=action))
      }
    }
  }

  def deletePost(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        AuthFile.delete(authority.id, authority)
        Redirect(routes.Search.list("authority"))
      }
    }
  }
}
