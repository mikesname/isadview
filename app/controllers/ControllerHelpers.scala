package controllers

import play.api._
import play.api.mvc._
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}
import play.api.libs.concurrent.Promise


/*
 * Wraps optionalUserAction to asyncronously fetch the User's profile.
 */
trait AuthController extends Controller with Auth with Authorizer {

  def optionalUserProfileAction(f: Option[User] => Request[AnyContent] => Result): Action[AnyContent] = {
    optionalUserAction { implicit userOption => implicit request => 
      userOption match {
        case Some(user) => {
          Async {            
            models.UserProfile.fetchByUserID(user.id).map { profileopt =>
              f(Some(user.withProfile(profileopt)))(request)
            }
          }
        }
        case None => f(userOption)(request)
      }
    }
  }

  def authorizedUserProfileAction(authority: Authority)(f: User => Request[AnyContent] => Result): Action[AnyContent] = {
    authorizedAction(authority) { implicit user => implicit request => 
      Async {
        models.UserProfile.fetchByUserID(user.id).map { profileopt =>
          f(user.withProfile(profileopt))(request)
        }
      }
    }
  }
}

/*
 * Possible way to start refactoring common patterns in controllers, using delegation
 */
trait Crud {
  self: AuthController =>
  def crudDetail[T](finder: (String => Promise[T]), view: (T => String), slug: String) = optionalUserProfileAction { 
      implicit maybeUser => implicit request =>
    Async {
      finder(slug).map { item =>
        Ok(view(item))
      }
    }
  }
}

trait ControllerHelpers {

  def isAjaxRequest[T](request: Request[T]): Boolean = {
      request.headers.get("X-REQUESTED-WITH").getOrElse("").toUpperCase() == "XMLHTTPREQUEST"
  }
  
  /*
   * Play Forms don't currently support multi-value select widgets. We 
   * need to transform the input from:
   *  key -> Seq(va1, val2, val3) to:
   *  key[0] -> Seq(val1), key[1] -> Seq(val2), key[2] -> Seq(val3)
   */
  def transformMultiSelects(formData: Option[Map[String,Seq[String]]], multies: List[String]) = {
    formData.map(b => {
      b.flatMap { (t: (String,Seq[String])) =>
        t match {
          case (n, s) if multies.contains(n) => {
            s.zipWithIndex.map(t => n + "[" + t._2 + "]" -> List(t._1))
          }
          case other => List(other)
        }
      }
    }).getOrElse(Map[String,Seq[String]]())
  }

}
