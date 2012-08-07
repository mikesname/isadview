package controllers

import scala.reflect.Manifest
import scalax.io.{Resource,Codec => IOCodec}

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import net.liftweb.json

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.CollectionForm



object Collections extends AuthController with ControllerHelpers {
  def detail(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Ok(views.html.collection.detail(collection, collection.description))
      }
    }
  }

  def create(repo: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    val action = routes.Collections.createPost(repo)
    Ok(views.html.collection.form(f=CollectionForm.form, action=action))
  }

  def createPost(repo: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    CollectionForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.collection.form(f=errorForm, action=routes.Collections.createPost(repo)))
      },
      data => {
        Async {
          Repository.fetchBySlug(repo).flatMap { repository =>
            Collection.create(new Collection(description=data)).flatMap { created =>
              Repository.createRelationship(created, repository, "heldBy").map { edge =>
                Redirect(routes.Collections.detail(slug=created.slug.get))
              }
            }
          }
        }
      }
    )
  }

  def update(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val form = CollectionForm.form.fill(collection.description)
        val action = routes.Collections.updatePost(slug)
        Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
      }
    }
  }

  def updatePost(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
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
            action=routes.Collections.updatePost(slug), c=Some(collection)))
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
  
  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val action = routes.Collections.deletePost(slug)
        Ok(views.html.basedelete(c=collection, action=action))
      }
    }
  }

  def deletePost(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Collection.delete(collection.id, collection)
        Redirect(routes.Search.list("collection"))
      }
    }
  }

  import importers.XMLPullParser

  def xmlToGeoff(slug: String, in: String, out: String) = optionalUserAction { implicit maybeUser => implicit request =>
    var format = request.queryString.getOrElse("format", Seq()).headOption.getOrElse(
          throw sys.error("No XML format argument supplied."))
    Async {
      Repository.fetchBySlug(slug).map { repo =>
        val output = Resource.fromFile(out)
        output.truncate(0)
        val importer: importers.Importer[xml.NodeSeq] = format match {
          case "doc" => importers.USHMM
          case "ead" => importers.EAD
          case x => sys.error("Unknown importer format: %s".format(x))
        }
        XMLPullParser.processSource(format, io.Source.fromFile(in)) { doc =>
          output.writeStrings(importer.docToGeoff("repo%d".format(repo.id), doc), separator="\n")(IOCodec.UTF8)
        }
        Ok("done")
      }
    }
  }

  /*
   * Temporary hack way of getting a Geoff file into Neo4j. The `f` parameter refers
   * to a file path relative to the app pwd.
   */
  def importGeoff(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>
    var file = request.queryString.getOrElse("f", Seq()).headOption.getOrElse(
          throw sys.error("No Geoff file supplied."))
    val size = 100000
    val timeout = 100000L

    // Let's crash the JVM...
    // The lines are an iterator, not all in memory at once...
    val lines = io.Source.fromFile(file).getLines
    val init = Map[String,Map[String,String]]()
    Async {
      models.Repository.fetchBySlug(slug).map { repository =>
        val out = lines.grouped(size).map(_.toList).foldLeft(init) { case(params, lineList) =>
          models.Repository.importGeoff(repository, lineList, params).await(timeout).get
        }
        Ok(generate(out))
      }
    }
  }
}
