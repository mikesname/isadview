package solr

trait SolrModel {
  def id: Long
  def slug: Option[String]
  def toSolrDoc: Map[String,Any]
}
