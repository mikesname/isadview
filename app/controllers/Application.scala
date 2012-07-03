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

  def index = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def login = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.login(form=UserForm.openid, action=routes.Application.loginPost))
  }

  def loginPost = optionalUserAction { implicit maybeUser => implicit request =>
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
            //Seq("email" -> "http://schema.openid.net/contact/email")
            Seq()
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
            case Some(assoc) =>
              gotoLoginSucceeded(assoc.user.get.id)
            case None => 
              Redirect(routes.Application.signupComplete).withSession("openid" -> info.id)
          }
        }
        case Thrown(t) => {
          // Here you should look at the error, and give feedback to the user
          Redirect(routes.Application.login).flashing("error" -> openidError)
        }
      })
    )
  }

  def signup = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.login(form=UserForm.openid, action=routes.Application.signupPost))
  }

  def signupPost = optionalUserAction { implicit maybeUser => implicit request =>
    UserForm.openid.bindFromRequest.fold(
      error => {
        Logger.info("bad request " + error.toString)
        BadRequest(views.html.login(form=error, action=routes.Application.signupPost))
      },
      {
        case (openid) => AsyncResult(
          OpenID.redirectURL(
            openid,
            routes.Application.openIDCallback.absoluteURL(),
            //Seq("email" -> "http://schema.openid.net/contact/email")
            Seq()
          )
            .extend( _.value match {
                case Redeemed(url) => Redirect(url)
                case Thrown(t) => Redirect(routes.Application.signup).flashing("error" -> openidError)
            }))
      }
    )
  }

  def signupComplete = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.signupInfo(form=UserForm.signupForm, action=routes.Application.signupCompletePost))
  }

  def signupCompletePost = optionalUserAction { implicit maybeUser => implicit request =>
    session.get("openid").map { openid =>
      UserForm.signupForm.bindFromRequest.fold(
        errorForm => {
          BadRequest(views.html.signupInfo(form=errorForm, action=routes.Application.signupCompletePost))
        },
        {
          case (name, email) => {
            models.sql.User.create(name, email) match {
              case Some(user) => {
                user.addAssociation(openid)
                request.session - "openid"
                gotoLoginSucceeded(user.id)
              }
              case None => BadRequest("Error creating user account.")
            }
          }
        }
      )
    }.getOrElse {
      Redirect(routes.Application.signup).flashing("error" -> "No ID!")
    }
  }

  def testq = Action {
    import neo4j.query.Query

    val q = Query()
    println(q)
    Ok("look at shell")
  }

  def dbtest = optionalUserAction { implicit maybeUser => request =>
    import models.sql.User
    Ok("Result: " + User.findAll)
  }
}
