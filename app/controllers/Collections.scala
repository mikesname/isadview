package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

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

  def edit(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val form = CollectionForm.form.fill(collection.description)
        val action = routes.Collections.save(slug)
        Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
      }
    }
  }

  def save(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
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
  
  def confirmDelete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val action = routes.Collections.delete(slug)
        Ok(views.html.basedelete(c=collection, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Collection.delete(collection.id, collection)
        Redirect(routes.Search.list("collection"))
      }
    }
  }

  import play.api.data._
  import play.api.data.Forms._
  import scala.xml._
  import scala.xml.pull._
  import scala.io.Source

  def importForm(repo: String) = authorizedAction(models.sql.Administrator) { user => implicit request =>
    Ok(views.html.importForm(user, routes.Collections.importPost(repo)))
  }

  def importPost(repo: String) = optionalUserAction(parse.xml) { implicit maybeUser => implicit request =>
    //request.body.file("file").map { xml =>
    //  val ev = new XMLEventReader(Source.fromFile(xml.ref.file))
    //  val out = ev.map(_.toString)
    //  Ok(out.mkString("\n"))
    //}.getOrElse(BadRequest("nope"))

    def optString(s: String) = if (s.trim.isEmpty) None else Some(s)

    def attributeValueEquals(value: String)(node: Node) = {
      node.attributes.exists(_.value.text == value)
    }

    def getField(field: String, elem: NodeSeq): Option[String] = {
      optString((elem \ "field").filter(attributeValueEquals(field)).text)
    }

    def multiFields(fields: List[String], sep: String, elem: NodeSeq) = {
      optString(fields.map { fname =>
        (elem \ "field").filter(attributeValueEquals(fname)).text
      }.filter(_.trim.nonEmpty).mkString("\n"))
    }

    val nodes: List[Map[String,Any]] = (request.body \ "doc").map { elem =>
      Map(
        "name"        -> getField("title", elem),
        "identifier"  -> getField("irn", elem),
        "source"  -> getField("acq_source", elem),
        "scope_and_content"  -> getField("scope_content", elem),
        "extent_and_medium"  -> getField("extent", elem),
        "legal_status"  -> getField("legal_status", elem),
        "acquisition"  -> multiFields(List("acq_source", "acccession_number", "acq_credit"), "\n", elem)
      )
    }.toList

    Ok(nodes.mkString(", "))
  }

  def updateIndex = authorizedAction(models.sql.Administrator) { user => implicit request =>
    import neo4j.query.Query
    import solr.SolrUpdater

    // Holy moly does this get confusing...
    Async {
      // First, take the initial async list of objects and get their
      // full representations, including relations
      val clist = Collection.query.get().map { list =>
        list.map(c => Collection.fetchBySlug(c.slug.get))
      }
      clist.map { cp =>
        Async {
          // Now take the List of Promises and convert them into
          // a Promise[List[models.Collection]] using the sequence
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
