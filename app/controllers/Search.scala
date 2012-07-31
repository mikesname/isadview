package controllers

import java.util.Locale

import play.api._
import play.api.mvc._
import play.api.libs.iteratee.{Concurrent,Enumerator}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import controllers._


object Search extends AuthController with ControllerHelpers {

  val ALL_SEARCH = "search"

  // FIXME: Work out out to get the preferred lang
  // from the application context somehow
  implicit val locale: Locale = new Locale("en", "GB")

  def home = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Ok(views.html.home(routes.Search.list(ALL_SEARCH)))
  }

  def list(rtype: String, page: Int, orderBy: Int, filter:String, field:String) = optionalUserProfileAction { implicit
      maybeUser => implicit request =>
    val index = if (rtype == ALL_SEARCH) None else Some(rtype)

    val listpromise = solr.models.Description.list(index=index, page=page, pageSize=20,
          orderBy=orderBy, field=field, query=filter, facets=request.queryString)

    Async {
      listpromise.map { page =>
        Ok(views.html.list(rtype, page, currentOrderBy=orderBy, currentFilter=filter))
      }
    }
  }

  def facets(facet:String, rtype: String, page: Int, sort: String, filter:String, field:String) = optionalUserProfileAction {
      implicit maybeUser => implicit request =>

    val index = if (rtype == ALL_SEARCH) None else Some(rtype)
    var fpagepromise = solr.models.Description.facet(
      facet=facet, index=index, page=page, sort=sort, query=filter, field=field, facets=request.queryString)
    Async {
      fpagepromise.map { fpage =>
        if(isAjaxRequest(request))
          Ok(views.html.facets_ajax(fpage, sort))
        else
          Ok(views.html.facets(fpage, sort))
      }
    }
  }

  def updateIndex = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.updateIndex(action=routes.Search.updateIndexPost))
  }

  def updateIndexPost = optionalUserAction { implicit maybeUser => implicit request =>
    import neo4j.query.Query
    import solr.SolrUpdater

    import play.api.data._
    import play.api.data.Forms._

    case class UpdateEntities(val collection: Boolean, val authority: Boolean, val repository: Boolean)

    Form(
      mapping(
        "collection" -> boolean,
        "authority" -> boolean,
        "repository" -> boolean
      )(UpdateEntities.apply)(UpdateEntities.unapply)
    ).bindFromRequest.fold(
      errorForm => {
        println(errorForm)
        BadRequest(views.html.updateIndex(action=routes.Search.updateIndexPost))
      },
      entities => {
        // TODO: Reduce this code dup and parallise!
        val cchannel = if (entities.collection) {
          Some(Concurrent.unicast[String]( onStart = channel => {
            solr.SolrUpdater.indexAll(models.Collection, channel)
          }, onComplete = ()))
        } else None
        val achannel = if (entities.authority) {
          Some(Concurrent.unicast[String]( onStart = channel => {
            solr.SolrUpdater.indexAll(models.Authority, channel)
          }, onComplete = ()))
        } else None
        val rchannel = if (entities.repository) {
          Some(Concurrent.unicast[String](onStart = channel => {
            solr.SolrUpdater.indexAll(models.Repository, channel)
          }, onComplete = ()))
        } else None
        val chans = List(cchannel, achannel, rchannel).flatMap(s => s)
        if (!chans.isEmpty)
          Ok.stream(chans.reduceLeft((s, c) => s.interleave(c)).andThen(Enumerator.eof))
        else
          Ok("Nothing to do")
      }
    )
  }
}

