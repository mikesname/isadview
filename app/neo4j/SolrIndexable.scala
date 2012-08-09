package neo4j

/*
 * Trait defining a model that mixes in SolrModel and Neo4jModel
 */
trait SolrIndexable extends solr.SolrModel with neo4j.Neo4jModel

