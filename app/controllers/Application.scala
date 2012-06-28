package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.openid._
import play.api.libs.concurrent._

object Application extends Controller {
  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def login = Action { implicit request =>
    Ok(views.html.login(action=routes.Application.loginPost))
  }

  def loginPost = Action { implicit request =>
    Form(single(
      "openid" -> nonEmptyText
    )).bindFromRequest.fold(
      error => {
        Logger.info("bad request " + error.toString)
        BadRequest(error.toString)
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
                case Thrown(t) => Redirect(routes.Application.login)
            }))
      }
    )
  }

  def openIDCallback = Action { implicit request =>
    AsyncResult(
      OpenID.verifiedId.extend( _.value match {
        case Redeemed(info) => Ok(info.id + "\n" + info.attributes)
        case Thrown(t) => {
          // Here you should look at the error, and give feedback to the user
          Redirect(routes.Application.login)
        }
      })
    )
  }

  def dbtest = Action { implicit request =>
    import play.api.db.DB
    import anorm.SQL
    import play.api.Play.current
    val result = DB.withConnection { implicit c =>
      var query = SQL("SELECT DISTINCT email FROM auth_user WHERE username = {user}").on("user" -> "mike")
      query.apply().head[String]("auth_user.email")
    }
    Ok("Result: %s".format(result))
  }
}
