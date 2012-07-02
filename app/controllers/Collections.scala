package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.CollectionForm



object Collections extends Controller with Auth with Authorizer with ControllerHelpers {
  def detail(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Async {
          Repository.findRelatedTo(collection, Repository.Direction.Out, "heldBy").map { repos =>
            // there should ALWAYS be a repository.
            val repo = repos.head
            Async {
              Authority.findRelatedTo(collection, Authority.Direction.Out, "createdBy").map { auths =>
                val creator = auths.headOption
                Ok(views.html.collection.detail(
                    collection, collection.description, repo, creator))
              }
            }
          }
        }
      }
    }
  }

  def new_ = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.collection.form(f=CollectionForm.form, action=routes.Collections.create))
  }

  def create = optionalUserAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    CollectionForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.collection.form(f=errorForm, action=routes.Collections.create))
      },
      data => {
        Async {
          Collection.create(new Collection(description=data)).map { created =>
            Redirect(routes.Collections.detail(slug=created.slug.get))
          }
        }
      }
    )
  }

  def edit(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val form = CollectionForm.form.fill(collection.description)
        val action = routes.Collections.save(slug)
        Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
      }
    }
  }

  def save(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
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
              Collection.persist(collection.id, collection.copy(description=data)).map { updated =>
                Redirect(routes.Collections.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }
  
  def confirmDelete(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val action = routes.Collections.delete(slug)
        Ok(views.html.basedelete(c=collection, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Collection.delete(collection.id, collection)
        Redirect(routes.Search.list("collection"))
      }
    }
  }
}
