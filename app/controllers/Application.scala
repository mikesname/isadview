package controllers

import play.api._
import play.api.mvc._

import solr.models._

import org.neo4j.graphdb._
import org.neo4j.rest.graphdb.RestGraphDatabase
import org.neo4j.scala._


object Application extends Controller with Neo4jWrapper with RestGraphDatabaseServiceProvider {

  override def uri = new java.net.URI("http://localhost:7474/db/data")
  override def userPw: Option[(String, String)] = None
  
  def isAjaxRequest[T](request: Request[T]): Boolean = {
      request.headers.get("X-REQUESTED-WITH").getOrElse("").toUpperCase() == "XMLHTTPREQUEST"
  }

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }
  
  def list(rtype: String, page: Int, orderBy: Int, filter:String, field:String) = Action { implicit request =>
    Ok(views.html.list(rtype, Description.list(
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
    val (fclass, flist) = Description.facet(
      facet=facet, index=Some(rtype), page=page, sort=sort, query=filter, field=field, facets=request.queryString)
    if(isAjaxRequest(request))
      Ok(views.html.facets_ajax(fclass, flist, page, sort))
    else
      Ok(views.html.facets(fclass, flist, page, sort))
  }

  def detail(rtype: String, slug:String) = Action { implicit request =>
    val node = withTx { implicit ds =>
      ds.gds.index().forNodes(rtype).query("slug", slug).getSingle()
    }
    Ok(views.html.detail(rtype=rtype, node=node))
  }
  
  
}
