package solr

trait SolrModel {
  def slug: Option[String]
  def toSolrDoc: Map[String,Any]
}
