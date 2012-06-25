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
}
