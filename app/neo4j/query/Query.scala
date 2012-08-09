/*
 * Crude version of Django functional querysets.
 *
 *
 */

package neo4j.query

import neo4j.GremlinHelper

import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext
import net.liftweb.json.JsonAST.JValue
import com.codahale.jerkson.Json

object Query {
  val lookupSep = "__"
  val defaultOp = "exact"
  val gremlinMethod = "query"
}

object QueryOrder extends Enumeration("ASC", "DESC") {
  type Order = Value
  val ASC, DESC = Value
}


case class Query[T](
  val builder: JValue => T,
  val indexName: String,
  val low: Int = 0,
  val high: Option[Int] = None,
  private val order: List[(String, QueryOrder.Order)] = Nil,
  private val filters: Map[String,String] = Map(),
  private val inrels: List[String] = Nil,
  private val outrels: List[String] = Nil
) extends GremlinHelper {
  private val defaultOrder = "id"
  def filter(kv: (String, String)*) = copy(filters = kv.foldLeft(filters)((f, k) => f + k))
  def compiledFilters = {
    filters.map { case(key, value) =>
      val keyparts = key.split(Query.lookupSep)
      // TODO: Check validity of operator ('exact', 'startswith', etc)
      if (keyparts.length > 1)
        List(keyparts(0), keyparts(1), value)
      else
        List(key, Query.defaultOp, value)
    }.toList
  }

  def orderBy(field: String, dir: QueryOrder.Order = QueryOrder.ASC) = copy(order = order ++ List(field -> dir))

  private def getOrder = {
    if (order.isEmpty)
      List(List(defaultOrder, QueryOrder.ASC.toString))
    else 
      order.map((t: (String, QueryOrder.Order)) => List(t._1, t._2.toString))
  }

  def params = Map(
    "index_name" -> indexName,
    "inrels" -> Nil,  // Not implemented
    "outrels" -> Nil, // Not implemented
    "filters" -> compiledFilters,
    "low" -> low,
    "high" -> high.getOrElse(null),
    "order_by" -> getOrder,
    "docount" -> false,
    "dodelete" -> false,
    "fields" -> Nil
  )             
  def apply(json: JValue) = builder(json)
  def slice(from: Int, to: Int) = copy(low=from, high=Some(to))
  def get(): Promise[List[T]] = {
    gremlin(Query.gremlinMethod, params).map { resp => 
      getJson(resp).children.map(apply(_))
    }
  }
  def getField(field: Any): Promise[List[String]] = {
    gremlin(Query.gremlinMethod, params + ("fields" -> List(field))).map { resp =>
      Json.parse[List[String]](resp.body)
    }
  }
  def getFields(fields: Any*): Promise[List[List[String]]] = {
    gremlin(Query.gremlinMethod, params + ("fields" -> fields)).map { resp =>
      Json.parse[List[List[String]]](resp.body)
    }
  }
  def count(): Promise[Int] = {
    gremlin(Query.gremlinMethod, params + ("docount" -> true)).map { resp =>
      Json.parse[Int](resp.body)
    }
  }
  def delete(): Promise[Boolean] = {
    gremlin(Query.gremlinMethod, params + ("dodelete" -> true)).map { resp =>
      Json.parse[Boolean](resp.body)
    }
  }
}

