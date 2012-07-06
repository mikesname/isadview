package solr

import com.codahale.jerkson.Json
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent._
import play.Play.application

object SolrUpdater {
  val batchSize = application.configuration.getInt("solr.update.batchSize")
  val solrBasePath = application.configuration.getString("solr.path")
  val updatePath = solrBasePath + "/update/json?wt=json&commit=true"
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )
  
  /*
   * Update a list of Solr models. The actual list is broken up
   * into batches of a fixed size so this function can accept
   * arbitrarily long lists.
   */
  def updateSolrModels(items: List[SolrModel]): Promise[List[Response]] = {
    Promise.sequence {
      items.grouped(batchSize).map { batch =>
        updateBatch(batch)
      }.toList
    }
  }

 /* Update a single batch of solr models.
  */ 
  def updateBatch(items: List[SolrModel]): Promise[Response] = {
    val data = items.map(_.toSolrDoc)
    WS.url(updatePath).withHeaders(headers.toList: _*).post(Json.generate(data))
  }
}
