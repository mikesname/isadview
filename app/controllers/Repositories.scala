package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import net.liftweb.json
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.RepositoryForm



object Repositories extends AuthController with ControllerHelpers {
  def detail(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repo =>
        Ok(views.html.repository.detail(repo=repo, repo.description))
      }
    }
  }

  def new_ = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Ok(views.html.repository.form(f=RepositoryForm.form, action=routes.Repositories.create))
  }

  def create = optionalUserProfileAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    RepositoryForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.repository.form(f=errorForm, action=routes.Repositories.create))
      },
      data => {
        Async {
          Repository.create(new Repository(description=data)).map { created =>
            Redirect(routes.Repositories.detail(slug=created.slug.get))
          }
        }
      }
    )
  }

  def edit(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        val form = RepositoryForm.form.fill(repository.description)
        val action = routes.Repositories.save(slug)
        Ok(views.html.repository.form(f=form, action=action, r=Some(repository)))
      }
    }
  }

  def save(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
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
              Repository.persist(repository.id, repository.copy(description=data)).map { updated =>
                Redirect(routes.Repositories.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }
  
  def confirmDelete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        val action = routes.Repositories.delete(slug)
        Ok(views.html.basedelete(c=repository, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Repository.fetchBySlug(slug).map { repository =>
        Repository.delete(repository.id, repository)
        Redirect(routes.Search.list("repository"))
      }
    }
  }
}
