package controllers

import play.api._
import play.api.mvc._

import solr.models._


object Application extends Controller {
  
  def index = Action { request =>
    Ok(views.html.index("Your new application is ready."))
  }
  
  def list(page: Int, orderBy: Int, filter:String) = Action { implicit request =>
    Ok(views.html.list(Description.list(page=page, pageSize=20, orderBy=orderBy, query=filter), 
        currentOrderBy=orderBy, currentFilter=filter))
  }
  
}
