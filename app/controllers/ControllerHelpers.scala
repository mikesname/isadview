package controllers

import play.api._
import play.api.mvc._

trait ControllerHelpers {
  def isAjaxRequest[T](request: Request[T]): Boolean = {
      request.headers.get("X-REQUESTED-WITH").getOrElse("").toUpperCase() == "XMLHTTPREQUEST"
  }
}
