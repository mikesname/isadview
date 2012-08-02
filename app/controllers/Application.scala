package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.i18n._
import play.api.data.Forms._
import play.api.libs.openid._
import play.api.libs.concurrent.execution.defaultContext
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
        case (openid) => AsyncResult {
          OpenID.redirectURL(
            openid,
            routes.Application.openIDCallback.absoluteURL(),
            //Seq("email" -> "http://schema.openid.net/contact/email")
            Seq()
          ).extend { _.value.get match {
              case Right(url) => Redirect(url)
              case Left(t) => Redirect(routes.Application.login).flashing("error" -> openidError)
            }
          }
        }
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
      OpenID.verifiedId.extend { _.value.get match {
          case Right(info) => {
            // check if there's a user with the right id
            Association.findByUrl(info.id) match {
              case Some(assoc) =>
                gotoLoginSucceeded(assoc.user.get.id)
              case None => 
                Redirect(routes.Application.signupComplete).withSession("openid" -> info.id)
            }
          }
          case Left(t) => {
            // Here you should look at the error, and give feedback to the user
            Redirect(routes.Application.login).flashing("error" -> openidError)
          }
        }
      }
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
            .extend( _.value.get match {
                case Right(url) => Redirect(url)
                case Left(t) => Redirect(routes.Application.signup).flashing("error" -> openidError)
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
                Async {
                  models.UserProfile.create0(new models.UserProfile(userId=user.id, data=new models.ProfileData())).map { created =>
                    gotoLoginSucceeded(user.id)
                  }
                }
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

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        VirtualCollections.saveItem,
        VirtualCollections.saveItemToNew
      )
    ).as("text/javascript")
  }

  // DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER!
  // NB: THIS METHOD IS A MASSIVE SECURITY HOLE AND SHOULD NOT BE EXPOSED TO THE WORLD!!!
  // DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER! DANGER!            
  def gremlinProxy(method: String) = optionalUserAction { implicit maybeUser => implicit request =>
    // Naively convert params into a more gremlin-like representation
    // If the Map[String,Seq] only has one item in it, convert it to
    // a Map[String,Any] using that first item.
    val params: Map[String,Any] = request.queryString.map { case (key, value) =>
      value match {
        case Seq(item) => (key, item)
        case _ => (key, value)
      }
    }

    Async {
      neo4j.GremlinHelper.gremlin(method, params).map { response =>
        Status(response.status)(response.body)
      }
    }
  }

  def testq = Action { implicit request =>
    import neo4j.query.Query

    var query = request.queryString.getOrElse("q", Seq()).headOption
    var field = request.queryString.getOrElse("field", Seq()).headOption.getOrElse("name")
    var op = request.queryString.getOrElse("op", Seq()).headOption.getOrElse("exact")
    val from = request.queryString.getOrElse("from", Seq()).headOption.map(_.toInt)
    val to = request.queryString.getOrElse("to", Seq()).headOption.map(_.toInt)
    val q = Query(models.Repository.apply _, models.Repository.indexName)
    println("Done query...")

    var res = q
    query.map { qstr =>
      res = res.filter("%s__%s".format(field, op) -> qstr)
    }
    if (to.isDefined && from.isDefined)
      res = res.slice(from.get, to.get.max(from.get))
    Async {
      res.get().map { out =>
        Ok("Result: %s\n\nCount: %d".format(out, out.length))
      }
    }
  }

  def dbtest = optionalUserAction { implicit maybeUser => request =>
    import models.sql.User
    Ok("Result: " + User.findAll)
  }
}
