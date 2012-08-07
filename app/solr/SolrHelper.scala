package solr

import play.Play.application


trait SolrHelper {
  val batchSize = application.configuration.getInt("solr.update.batchSize")
  val solrBasePath = application.configuration.getString("solr.path")
  val updatePath = solrBasePath + "/update/json?wt=json&commit=true"
  val headers = Map(
    "Accept" -> "application/json",
    "Content-Type" -> "application/json; charset=utf8"
  )
}


// vim: set ts=4 sw=4 et:
