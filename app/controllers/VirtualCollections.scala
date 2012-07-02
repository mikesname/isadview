package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import com.codahale.jerkson.Json._

import models._
import forms.UserForm


object VirtualCollections extends Controller with Auth with Authorizer with ControllerHelpers {
  def detail(slug: String) = authorizedAction(models.sql.NormalUser) { implicit user => implicit request =>
    Async {
      VirtualCollection.fetchBySlug(slug).map { vc =>
        Ok(views.html.virtualcollection.detail(user, vc))
      }
    }
  }

  def new_ = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    Ok(views.html.user.vcform(
        user, UserForm.virtualCollection, routes.VirtualCollections.create))
  }

  def create = authorizedAction(models.sql.NormalUser) { user => implicit request => 
    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        UserForm.virtualCollection.bindFromRequest.fold(
          errorForm => BadRequest(views.html.user.vcform(
              user, errorForm, routes.VirtualCollections.create)),
          newvc => {
            // TODO: if we don't already have a profile, create one
            val profile = profileopt.getOrElse(
                throw play.api.PlayException("Missing profile", "No profile found for user."))
            createNew(profile, newvc)
          }
        )
      }
    }
  }

  private def createNew(profile: UserProfile, vcdesc: VirtualCollectionDescription) = {
    Async {
      UserProfile.createVirtualCollection(profile, vcdesc).map { created =>
        Redirect(routes.Users.profile)
      }
    }
  }
}
