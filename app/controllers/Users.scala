package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise

import net.liftweb.json
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import com.codahale.jerkson.Json._

import models.{UserProfile, ProfileData}
import forms.UserForm


object Users extends Controller with Auth with Authorizer with ControllerHelpers {
  def profile = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    Async {
      UserProfile.fetchByFieldOption("user_id", user.id.toString).map { profileopt =>
        val profile = profileopt.map(_.data).getOrElse(new ProfileData())
        val form = UserForm.profileForm.fill(profile)
        Ok(views.html.user.detail(user, profile))
      }
    }
  }

  //def new_ = Action { implicit request => 
  //  Ok(views.html.user.form(f=UserForm.signupForm, action=routes.Users.create))
  //}

  //def create = Action { implicit request =>
  //  // transform input for multiselects
  //  val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
  //    "profile.languages"
  //  ))

  //  UserForm.signupForm.bindFromRequest().fold(
  //    errorForm => {
  //      BadRequest(
  //        views.html.user.form(f=errorForm, action=routes.Users.create))
  //    },
  //    data => {
  //      Async {
  //        auth = new UserAuth(username,  
  //        User.create(new User(profile=data)).map { created =>
  //          Redirect(routes.Authorities.detail(slug=created.slug.get))
  //        }
  //      }
  //    }
  //  )
  //}
}
