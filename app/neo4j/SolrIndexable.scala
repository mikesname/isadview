package neo4j

/*
 * Trait defining a model that mixes in SolrModel and Model
 */
trait SolrIndexable extends solr.SolrModel with neo4j.Model {
  def summary: Option[String]
  def details: Map[String, Any] = Map()
}

