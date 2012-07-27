package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

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

  def updateIndex = optionalUserAction { implicit maybeUser => implicit request =>
    import neo4j.query.Query
    import solr.SolrUpdater

    // Holy moly does this get confusing...
    Async {
      // First, take the initial async list of objects and get their
      // full representations, including relations
      val clist = Repository.query.get().map { list =>
        list.map(c => Repository.fetchBySlug(c.slug.get))
      }
      clist.map { cp =>
        Async {
          // Now take the List of Promises and convert them into
          // a Promise[List[models.Repository]] using the sequence
          // function.
          Promise.sequence(cp).flatMap { items =>
            SolrUpdater.updateSolrModels(items).map { alldone =>
              Ok("%s".format(alldone.map(r => "%s\n".format(r.body))))  
            }
          }
        }
      }
    }
  }
}
