package solr

import com.codahale.jerkson.Json
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent._
import play.api.libs.concurrent.execution.defaultContext
import play.api.libs.iteratee.Concurrent


object SolrUpdater extends SolrHelper {
  /*
   * Update all objects handled by the given data access object, sending
   * progress back through the given channel enumerator.
   * Updates are performed asyncronously in batches to prevent overloading
   * the database.
   */
  def indexAll[T <: neo4j.SolrIndexable](dao: neo4j.DataSource[T], channel: Concurrent.Channel[String]) = {
    dao.query.count().map { count =>
      channel.push("Updating %s index (items: %d)\n".format(dao.indexName, count))
      val batches: List[Promise[List[Response]]] = (0 until count).grouped(batchSize).toList.map { range =>
        val start = range.head
        val end = range.last
        // FIXME: Get a list of models. This doesn't include related objects,
        // so (currently) we need to requery for the complete item.
        dao.query.slice(start, end).getField("slug").flatMap { slugs =>
          // Get a list of Promise[T]
          channel.push("Fetching full data for %s: %d to %d\n".format(dao.indexName, start, end))
          val plist = slugs.filter(!_.isEmpty).map(dao.fetchBySlug(_)) 
          // Turn it into a promise of List[T]
          Promise.sequence(plist).flatMap { full =>
            SolrUpdater.updateSolrModels(full).map { r =>
              val msg = "DONE %s index: %d to %d\n".format(dao.indexName, start, end)
              print(msg)
              channel.push(msg)
              r
            }
          }
        }
      }
      // Wait on all the batches to complete and when done,
      // close the channel
      Promise.sequence(batches).map { fini =>
        channel.end()
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

  def updateSolrModel(item: SolrModel) = updateSolrModels(List(item))

 /* Update a single batch of solr models.
  */ 
  def updateBatch(items: List[SolrModel]): Promise[Response] = {
    val data = items.map(_.toSolrDoc)
    WS.url(updatePath).withHeaders(headers.toList: _*).post(Json.generate(data))
  }
}
