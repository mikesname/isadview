package solr

import com.codahale.jerkson.Json
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent._
import play.api.libs.iteratee._
import play.Play.application

object SolrUpdater {
  val batchSize = application.configuration.getInt("solr.update.batchSize")
  val solrBasePath = application.configuration.getString("solr.path")
  val updatePath = solrBasePath + "/update/json?wt=json&commit=true"
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )
  
  def indexAll[T <: solr.SolrModel](dao: neo4j.data.Neo4jDataSource[T], pushee: Enumerator.Pushee[String]) = {
    dao.query.count().map { count =>
      println("Updating %s index (items: %d)".format(dao.indexName, count))
      for (range <- (0 to count).grouped(batchSize)) {
        range.headOption.map { start =>
          val end = range.lastOption.getOrElse(start)
          dao.query.slice(start, end).get().map { partials =>
            val plist = partials.flatMap(_.slug).map(dao.fetchBySlug(_)) 
            Promise.sequence(plist).map { full =>
              SolrUpdater.updateSolrModels(full).map { r =>
                val msg = "Updated %ss: %d to %d\n".format(dao.indexName, start, end)
                print(msg)
                pushee.push(msg)
                r
              }
            }
          }
        }
      }
    }
  }

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
