package neo4j

import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

/*
 * Entity represented by a Neo4j object with its own index.
 */
trait IndexedEntity {
  self: GremlinHelper =>
  /*
   * Implementing objects must specify this as the TypeKey
   * and the name of vertex indexes.
   */
  val TypeKey = "element_type"
  val indexName: String
  val gremlinMethod: String
  
  def initialize(): Promise[Boolean] = {
    val params = Map(TypeKey -> indexName, "index_params" -> None)
    gremlin(gremlinMethod, params).map { resp =>
      // TODO: Actually handle the respon
      true
    }
  }
}

trait IndexedVertex extends IndexedEntity {
  self: GremlinHelper =>
  override val gremlinMethod = "get_or_create_vertex_index"
}

trait IndexedEdge extends IndexedEntity {
  self: GremlinHelper =>
  override val gremlinMethod = "get_or_create_edge_index"
}


