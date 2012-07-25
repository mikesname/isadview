package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

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
          Collection.findRelatedTo(auth, Collection.Direction.In, "createdBy").map { createdCollections =>
            Async {
              Collection.findRelatedTo(auth, Collection.Direction.Out, "mentionedIn").map { mentionedCollections =>
                Ok(views.html.authority.detail(auth, auth.description, createdCollections, mentionedCollections))
              }
            }
          }
        }
      }
    }
  }

  def new_ = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Ok(views.html.authority.form(f=AuthorityForm.form, action=routes.Authorities.create))
  }

  def create = optionalUserProfileAction { implicit maybeUser => implicit request =>
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

  def edit(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val form = AuthorityForm.form.fill(authority.description)
        val action = routes.Authorities.save(slug)
        Ok(views.html.authority.form(f=form, action=action, r=Some(authority)))
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

  def confirmDelete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        val action = routes.Authorities.delete(slug)
        Ok(views.html.basedelete(c=authority, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      AuthFile.fetchBySlug(slug).map { authority =>
        AuthFile.delete(authority.id, authority)
        Redirect(routes.Search.list("authority"))
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
      val clist = AuthFile.query.get().map { list =>
        list.map(c => AuthFile.fetchBySlug(c.slug.get))
      }
      clist.map { cp =>
        Async {
          // Now take the List of Promises and convert them into
          // a Promise[List[models.Authority]] using the sequence
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
