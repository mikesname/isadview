package solr

trait SolrModel {
  def toSolrDoc: Map[String,Any]
}
