package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json

import com.codahale.jerkson.Json._

import neo4j.models.{Repository,Contact,Collection,FuzzyDate,Authority}
import neo4j.forms.CollectionForm



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
}
