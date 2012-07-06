package controllers

import java.util.Locale

import play.api._
import play.api.mvc._
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import controllers._


object Search extends Controller with Auth with Authorizer with ControllerHelpers {

  // FIXME: Work out out to get the preferred lang
  // from the application context somehow
  implicit val locale: Locale = new Locale("de", "GB")

  def index = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }
  
  def list(rtype: String, page: Int, orderBy: Int, filter:String, field:String) = optionalUserAction { implicit
      maybeUser => implicit request =>
    Ok(views.html.list(rtype, solr.models.Description.list(
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

  def facets(facet:String, rtype: String, page: Int, sort: String, filter:String, field:String) = optionalUserAction {
      implicit maybeUser => implicit request =>
    var fpage = solr.models.Description.facet(
      facet=facet, index=Some(rtype), page=page, sort=sort, query=filter, field=field, facets=request.queryString)
    if(isAjaxRequest(request))
      Ok(views.html.facets_ajax(fpage, sort))
    else
      Ok(views.html.facets(fpage, sort))
  }
}

