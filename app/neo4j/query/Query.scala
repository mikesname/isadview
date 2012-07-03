/*
 * Crude version of Django functional querysets.
 *
 *
 */

package neo4j.query

import neo4j.data._

import models.Repository

object Query {
    val LOOKUP_SEP = "__"
    val ORDER_PATTERN = "\\?|[-+]/[.\\w]+$".r
    val ORDER_DIR = Map(
      "ASC" -> ("ASC", "DESC"),
      "DESC" -> ("DESC", "ASC")
    )
}


case class Query[A <: Neo4jDataSource[A]](
    val low: Int = 0,
    val high: Option[Int] = None,
    val order: List[String] = Nil,
    val filters: Map[String,String] = Map(),
    private val inrels: List[String] = Nil,
    private val outrels: List[String] = Nil
) extends collection.Iterable[A] {
    type Model = A
    private def runQuery = {
      val params = Map(
        "index_name" -> Repository.indexName,
        "inrels" -> Nil,
        "outrels" -> Nil,
        "filters" -> List(List("name", "startswith", "Wiener")),
        "low" -> 0,
        "high" -> 20,
        "order_by" -> List(),
        "docount" -> false,
        "dodelete" -> false
      )

      Repository.gremlin("query", params).map { resp => 
        println(resp.body)
        //list(getJson(resp))
        Nil
      }.await.get
    }

    private lazy val data: List[A] = runQuery

    override def iterator = data.iterator
}

