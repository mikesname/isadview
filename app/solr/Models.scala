package solr.models

import com.github.seratch.scalikesolr._
import com.github.seratch.scalikesolr.request.query.Query
import com.github.seratch.scalikesolr.request.query.facet

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

trait FClass

/**
 * Encapulates rendering a facet to the response. Transforms
 * various Solr-internal values into i18n and human-readable ones.
 *
 * @param key     the name of the Solr field being faceted on
 * @param name    the 'pretty' human name of the Solr field
 * @param param   the name of the HTTP param used to apply this facet
 * @param render  a function (String => String) used to transform the
 *                facet values into human-readable ones, using, for
 *                example, i18n lookups.
 */

case class FacetClass(
  key: String,
  name: String,
  param: String,
  render: (String) => String = s=>s) extends FClass
{

}


/**
 * Encapsulates a single facet.
 *
 * @param klass   the `FacetClass` to which this facet belongs.
 * @param value   the value of this facet
 * @param count   the number of objects to which this facet applies
 * @param applied whether or not this facet is activated in the response
 * @param desc    a more verbose description of this facet value
 */
case class Facet(klass: FClass, value: String, count: Long, applied: Boolean = false, desc: String = "") {

}

class FacetProcessor(facets: response.query.Facet) {
  
}


object Description {
  def list(index: Option[String] = None, page: Int = 0, pageSize: Int = 20, orderBy: Int = 1, query: String = ""): Page[Description] = {
    val offset = page * pageSize
    var squery = index match {
      case Some(x) => "django_ct:portal." + x
      case None => "*:*"
    }

    val client = Solr.httpServer(new java.net.URL("http://localhost:8983/solr")).newClient
    val req = new QueryRequest(query=Query(squery))
    req.facet = new facet.FacetParams(
      enabled=true, 
      params=List(new facet.FacetParam(facet.Param("facet.field"), facet.Value("django_ct")))
    )

    req.setStartRow(request.query.StartRow(offset))
    req.setMaximumRowsReturned(request.query.MaximumRowsReturned(pageSize))

    val response = client.doQuery(req)
    // We only care about documents with the following content types,
    // so use a flatMap to extract them into the correct classes
    Page(response.response.documents.flatMap(d => {
      d.get("django_ct").toString match {
        case "portal.repository" => List(d.bind(classOf[Repository]))
        case "portal.collection" => List(d.bind(classOf[Collection]))
        case "portal.authority" => List(d.bind(classOf[Authority]))
        case _ => Nil
      }
    }), page, offset, response.response.numFound)
  }
}

abstract trait Description {
  var id: String
  var django_ct: String
  var name: String
  var slug: String
  var other_names: List[String]
  var text: String
  var publication_date: String
  var publication_status: Int

  def index: String = django_ct.split("\\.")(1)
}

case class Repository(
    var id: String = "",
    var django_ct: String = "",
    var name: String = "",
    var slug: String = "",
    var description: String = "",
    var other_names: List[String] = Nil,
    var address: String = "",
    var country: String = "",
    var location: String = "",
    var text: String = "",
    var publication_date: String = "", // FIXME: Should be a date
    var publication_status: Int = 0
 ) extends Description {
    def this() = this("", "", "", "", "", Nil, "", "", "", "", "", 0)
    override def toString = "<%s: %s>".format(getClass, slug)
}

case class Authority(
    var id: String = "",
    var django_ct: String = "",
    var name: String = "",
    var slug: String = "",
    var history: String = "",
    var general_context: String = "",
    var other_names: List[String] = Nil,
    var type_of_entity: String = "",
    var text: String = "",
    var publication_date: String = "", // FIXME: Should be a date
    var publication_status: Int = 0
 ) extends Description {
    def this() = this("", "", "", "", "", "", Nil, "", "", "", 0)
    override def toString = "<%s: %s>".format(getClass, slug)
}

case class Collection(
    var id: String = "",
    var django_ct: String = "",
    var name: String = "",
    var slug: String = "",
    var description: String = "",
    var other_names: List[String] = Nil,
    var repository: String = "",
    var repository_slug: String = "",
    var location_of_materials: String = "",
    var tags: List[String] = Nil,
    var start_date: String = "", // FIXME: Should be a date
    var end_date: String = "", // FIXME: Should be a date
    var years: List[Int] = Nil,
    var dates: List[String] = Nil, // FIXME: Should be a date
    var dates_exact: List[String] = Nil, // FIXME: Should be a date
    var date_range: String = "",
    var text: String = "",
    var publication_date: String = "", // FIXME: Should be a date
    var publication_status: Int = 0,
    var languages_of_description: List[String] = Nil
 ) extends Description {
    def this() = this("", "", "", "", "", Nil, "", "", "", Nil, "", "", Nil, Nil, Nil, "", "", "", 0, Nil)
    override def toString = "<%s: %s>".format(getClass, slug)
}


