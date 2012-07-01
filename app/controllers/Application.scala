package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.i18n._
import play.api.data.Forms._
import play.api.libs.openid._
import play.api.libs.concurrent._

import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import forms.UserForm


object Application extends Controller with Auth with LoginLogout with Authorizer {

  val openidError = """
    |There was an error connecting to your OpenID provider.""".stripMargin

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def login = Action { implicit request =>
    Ok(views.html.login(form=UserForm.openid, action=routes.Application.loginPost))
  }

  def loginPost = Action { implicit request =>
    UserForm.openid.bindFromRequest.fold(
      error => {
        Logger.info("bad request " + error.toString)
        BadRequest(views.html.login(form=error, action=routes.Application.loginPost))
      },
      {
        case (openid) => AsyncResult(
          OpenID.redirectURL(
            openid,
            routes.Application.openIDCallback.absoluteURL(),
            Seq("email" -> "http://schema.openid.net/contact/email")
          )
            .extend( _.value match {
                case Redeemed(url) => Redirect(url)
                case Thrown(t) => Redirect(routes.Application.login).flashing("error" -> openidError)
            }))
      }
    )
  }

  def logout = Action { implicit request =>
    // do something...
    gotoLogoutSucceeded
  }

  def openIDCallback = Action { implicit request =>
    import models.sql.Association
    AsyncResult(
      OpenID.verifiedId.extend( _.value match {
        case Redeemed(info) => {
          // check if there's a user with the right id
          Association.findByUrl(info.id) match {
            case Some(assoc) => {
              gotoLoginSucceeded(assoc.user.get.id)
            }
            case None => Redirect(routes.Application.login).flashing(
                "error" -> Messages("No account matches that OpenID. Please sign-up to join."))
          }
        }
        case Thrown(t) => {
          // Here you should look at the error, and give feedback to the user
          Redirect(routes.Application.login).flashing("error" -> openidError)
        }
      })
    )
  }

  def dbtest = authorizedAction(models.sql.NormalUser) { user => implicit request =>
    import models.sql.User

    val result = User.findById(1L)

    Ok("Result: %s: %s".format(result, result.map(_.associations)))
  }
}
