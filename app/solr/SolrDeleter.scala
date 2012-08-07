package solr

import com.codahale.jerkson.Json
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent._
import play.api.libs.concurrent.execution.defaultContext
import play.api.libs.iteratee.Concurrent


object SolrDeleter extends SolrHelper {
  def deleteSolrModel(item: SolrModel): Promise[Response] = {
    assert(item.id > 0, "Solr model has no ID set!")
    val data = Map("delete" -> Map("id" -> item.id.toString))
    WS.url(updatePath).withHeaders(headers.toList: _*).post(Json.generate(data))
  }
}
