package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
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
