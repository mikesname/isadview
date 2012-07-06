package controllers

import java.util.Locale

import play.api._
import play.api.mvc._
import jp.t2v.lab.play20.auth.{Auth,LoginLogout}

import controllers._


object Search extends Controller with Auth with Authorizer with ControllerHelpers {

  val ALL_SEARCH = "search"

  // FIXME: Work out out to get the preferred lang
  // from the application context somehow
  implicit val locale: Locale = new Locale("de", "GB")

  def index = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def list(rtype: String, page: Int, orderBy: Int, filter:String, field:String) = optionalUserAction { implicit
      maybeUser => implicit request =>
    println("Search for items with rtype: " + rtype)
    val index = if (rtype == ALL_SEARCH) None else Some(rtype)
    Ok(views.html.list(rtype, solr.models.Description.list(
        index=index,
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

    val index = if (rtype == ALL_SEARCH) None else Some(rtype)
    var fpage = solr.models.Description.facet(
      facet=facet, index=index, page=page, sort=sort, query=filter, field=field, facets=request.queryString)
    if(isAjaxRequest(request))
      Ok(views.html.facets_ajax(fpage, sort))
    else
      Ok(views.html.facets(fpage, sort))
  }
}

