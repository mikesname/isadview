/*
 * Crude version of Django functional querysets.
 *
 *
 */

package neo4j.query

import neo4j.data._

import play.api.libs.concurrent.Promise
import net.liftweb.json.JsonAST.JValue
import com.codahale.jerkson.Json.parse

object Query {
  val LOOKUP_SEP = "__"
  val ORDER_PATTERN = "\\?|[-+]/[.\\w]+$".r
  val ORDER_DIR = Map(
    "ASC" -> ("ASC", "DESC"),
    "DESC" -> ("DESC", "ASC")
  )
  val DEFAULT_OP = "exact"
}


case class Query[A](
  val builder: JValue => A,
  val indexName: String,
  val low: Int = 0,
  val high: Option[Int] = None,
  val order: List[String] = Nil,
  private val filters: Map[String,String] = Map(),
  private val inrels: List[String] = Nil,
  private val outrels: List[String] = Nil
) extends GremlinHelper {
  def filter(kv: (String, String)*) = copy(filters = kv.foldLeft(filters)((f, k) => f + k))
  def compiledFilters = {
    val f = filters.map { case(key, value) =>
      val keyparts = key.split(Query.LOOKUP_SEP)
      // TODO: Check validity of operator ('exact', 'startswith', etc)
      if (keyparts.length > 1)
        List(keyparts(0), keyparts(1), value)
      else
        List(key, Query.DEFAULT_OP, value)
    }.toList
    println("Compiled filters: " + f)
    f
  }

  def params = Map(
    "index_name" -> indexName,
    "inrels" -> Nil,
    "outrels" -> Nil,
    "filters" -> compiledFilters,
    "low" -> low,
    "high" -> high.getOrElse(null),
    "order_by" -> List(),
    "docount" -> false,
    "dodelete" -> false
  )             
  def apply(json: JValue) = builder(json)
  def slice(from: Int, to: Int) = copy(low=from, high=Some(to))
  def get() = {
    gremlin("query", params).map { resp => 
      getJson(resp).children.map(apply(_))
    }
  }
  def count(): Promise[Int] = {
    gremlin("query", params + ("docount" -> true)).map { resp =>
      resp.body.toInt  
    }
  }
  def delete(): Promise[Boolean] = {
    gremlin("query", params + ("dodelete" -> true)).map { resp =>
      resp.body.toBoolean  
    }
  }
}

