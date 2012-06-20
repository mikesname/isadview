package controllers

import java.util.Locale

import play.api._
import play.api.mvc._

import controllers._
import solr.models


object Search extends Controller with ControllerHelpers {

  // FIXME: Work out out to get the preferred lang
  // from the application context somehow
  implicit val locale: Locale = new Locale("de", "GB")

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }
  
  def list(rtype: String, page: Int, orderBy: Int, filter:String, field:String) = Action { implicit request =>
    Ok(views.html.list(rtype, models.Description.list(
        index=Some(rtype),
        page=page,
        pageSize=20,
        orderBy=orderBy,
        field=field,
        query=filter,
        facets=request.queryString
      ), currentOrderBy=orderBy, currentFilter=filter)
    )
  }

  def facets(facet:String, rtype: String, page: Int, sort: String, filter:String, field:String) = Action { implicit request =>
    var fpage = models.Description.facet(
      facet=facet, index=Some(rtype), page=page, sort=sort, query=filter, field=field, facets=request.queryString)
    if(isAjaxRequest(request))
      Ok(views.html.facets_ajax(fpage, sort))
    else
      Ok(views.html.facets(fpage, sort))
  }
}

