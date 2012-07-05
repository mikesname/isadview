package solr

import com.codahale.jerkson.Json._
import com.github.seratch.scalikesolr._
import com.github.seratch.scalikesolr.request.common.WriterType
import com.github.seratch.scalikesolr.response.QueryResponse

object SolrUpdater {

  def updateDocs(docs: List[Map[String,Any]]) = {
    val client = Solr.httpServer(new java.net.URL(play.Play.application.configuration.getString("solr.path"))).newClient
    val solrdocs = docs.map { doc =>
      SolrDocument(
        writerType = WriterType.JSON,
        rawBody = generate(doc)
      )
    }
    println("Num docs: " + solrdocs.length)
    try {
      val req = new request.UpdateRequest(writerType=WriterType.JSON, documents=solrdocs)
      val res = client.doUpdateDocuments(req)
      client.doCommit()
      println(res.rawBody)
      res.rawBody

    } catch {
      case e: NullPointerException => "NULL POINTER, WHY!???: %s".format(solrdocs)
      case e: java.io.IOException => "IO EXCEPTION: WHY!???: \n\n%s\n\n".format(solrdocs)
      case other => throw other
    }
  }
}
