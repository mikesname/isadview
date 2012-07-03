/*
 * Crude version of Django functional querysets.
 *
 *
 */

package neo4j.query

import neo4j.data._

import models.Repository
import net.liftweb.json.JsonAST.JValue

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
) extends collection.Seq[A] with GremlinHelper {
  def filter(kv: (String, String)*) = copy(filters = kv.foldLeft(filters)((f, k) => f + k))

  def compiledFilters = {
    val f = filters.map { case(key, value) =>
      val keyparts = key.split(Query.LOOKUP_SEP)
      // TODO: Check validity of specifier
      if (keyparts.length > 1)
        List(keyparts(0), keyparts(1), value)
      else
        List(key, Query.DEFAULT_OP, value)
    }.toList
    println("Compiled filters: " + f)
    f
  }

  def apply(json: JValue) = builder(json)
  def apply(i: Int) = data(i)
  def length = data.length
  override def slice(from: Int, to: Int) = copy(low=from, high=Some(to))
  private def runQuery = {
    val params = Map(
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
    println("Calculating response..." + params)
    gremlin("query", params).map { resp => 
      println(resp.body)
      getJson(resp).children.map(apply(_))
    }.await.get
  }

  private lazy val data: List[A] = runQuery

  override def iterator = data.iterator
}

