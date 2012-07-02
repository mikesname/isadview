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


object Users extends Controller with Auth with Authorizer with ControllerHelpers {
  def profile = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        val profile = profileopt.getOrElse(UserProfile(user.id))
        Ok(views.html.user.detail(user, profile))
      }
    }
  }

  def edit = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        val profile = profileopt.map(_.data).getOrElse(new ProfileData())
        val form = UserForm.profileForm.fill(profile)
        val action = routes.Users.editPost
        Ok(views.html.user.form(user, form, action, profile))
      }
    }
  }

  def editPost = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "languages"
    ))

    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        val profile = profileopt.map(_.data).getOrElse(new ProfileData())
        UserForm.profileForm.bindFromRequest(formData).fold(
          errorForm => BadRequest(views.html.user.form(user, errorForm, routes.Users.editPost, profile)),
          profiledata => {
            profileopt match {
              case Some(profile) => {
                updateProfile(user, profile.copy(data=profiledata))
              }
              case None => {
                createProfile(user, profiledata)
              }
            }
          }
        )
      }
    }
  }  

  private def createProfile(user: models.sql.User, data: ProfileData) = {
    Async {
      UserProfile.create0(new UserProfile(userId=user.id, data=data)).map { created =>
        Redirect(routes.Users.profile)
      }
    }
  }

  private def updateProfile(user: models.sql.User, profile: UserProfile) = {
    Async {
      UserProfile.persist(profile.id, profile).map { updated =>
        Redirect(routes.Users.profile)
      }
    }
  }

  def newVC = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    Ok(views.html.user.vcform(
        user, UserForm.virtualCollection, routes.Users.newVCPost))
  }

  def newVCPost = authorizedAction(models.sql.NormalUser) { user => implicit request => 
    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        UserForm.virtualCollection.bindFromRequest.fold(
          errorForm => BadRequest(views.html.user.vcform(
              user, errorForm, routes.Users.newVCPost)),
          newvc => {
            // TODO: if we don't already have a profile, create one
            val profile = profileopt.getOrElse(
                throw play.api.PlayException("Missing profile", "No profile found for user."))
            createNewVC(profile, newvc)
          }
        )
      }
    }
  }

  private def createNewVC(profile: UserProfile, vcdesc: VirtualCollectionDescription) = {
    Async {
      UserProfile.createVirtualCollection(profile, vcdesc).map { created =>
        Redirect(routes.Users.profile)
      }
    }
  }
}
