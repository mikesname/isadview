package solr.models

import com.github.seratch.scalikesolr._
import com.github.seratch.scalikesolr.response.QueryResponse
import com.github.seratch.scalikesolr.request.query.{Query, FilterQuery, QueryParserType}
import com.github.seratch.scalikesolr.request.query.facet

import play.api.i18n

object Utils {

  def joinQueryString(qs: Map[String, Seq[String]]): String = {
    import java.net.URLEncoder
    qs.map { case (key, vals) => {
      vals.map(v => "%s=%s".format(key, URLEncoder.encode(v, "UTF-8")))    
    }}.flatten.mkString("&")
  }

  def pathWithoutFacet(fc: FacetClass, f: Facet, path: String, qs: Map[String, Seq[String]]): String = {
    List(path, joinQueryString(qs.map(qv => {
      qv._1 match {
        case fc.param => (qv._1, qv._2.filter(_!=f.paramVal))
        case _ => qv
      }
    }))).filter(_!="").mkString("?")
  }

  def pathWithFacet(fc: FacetClass, f: Facet, path: String, qs: Map[String, Seq[String]]): String = {
    path + "?" + joinQueryString(
      if (qs.contains(fc.param)) {
        qs.map(qv => {
          qv._1 match {
            case fc.param => (qv._1, qv._2.union(Seq(f.paramVal)).distinct)
            case _ => qv
          }
        })
      } else {
        qs.updated(fc.param, Seq(f.paramVal))
      }
    )
  }
}

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long, facets: List[FacetClass]) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

// Scala's enum-like Phantom types for defining
// how facets are ordered
case object OrderedByName extends Enumeration
case object OrderedByCount extends Enumeration

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
 * @param facets  a list of individual Facet values
 */
sealed abstract class FacetClass (
  val key: String,
  val name: String,
  val param: String,
  val render: (String) => String = s => s,
  private val facets: List[Facet] = Nil,
  val sort: Enumeration = OrderedByCount
)
{
  val fieldType: String
  
  def count: Int = facets.length
  
  def filtered: List[Facet] = facets.filter(_.count > 0)
  
  def sorted: List[Facet] = sort match {
    case OrderedByName => filtered.sortWith((a, b) => a.paramVal < b.paramVal) 
    case OrderedByCount => filtered.sortWith((a, b) => a.count < b.count) 
    case _ => filtered
  }
  
  def asParams: List[facet.FacetParam]
  
  def populateFromSolr(data: xml.Elem, current: Map[String,Seq[String]]): FacetClass
  
  def pretty(f: Facet): String = f.humanVal match {
    case Some(desc) => render(desc)
    case None => render(f.paramVal)
  }
}

case class FieldFacetClass(
  override val key: String,
  override val name: String,
  override val param: String,
  override val render: (String) => String = s=>s,
  val facets: List[Facet] = Nil,
  override val sort: Enumeration = OrderedByCount
) extends FacetClass(key,name, param,render,facets,sort) {
  override val fieldType: String = "facet.field"
  def asParams: List[facet.FacetParam] = {
    List(new facet.FacetParam(
      facet.Param(fieldType),
      facet.Value(key)
    ))      
  }
  override def populateFromSolr(data: xml.Elem, current: Map[String,Seq[String]]): FacetClass = {
    val applied = current.getOrElse(param, Seq[String]())
    val nodes = data.descendant.filter(n => (n \ "@name").text == "facet_fields") 
    var facets = List[Facet]()
    if (nodes.length > 0) {
      val my = nodes.head.descendant.filter(n => (n \ "@name").text == key)
      my.head.descendant.foreach(n => {
        val name = n \ "@name"
        if (name.length > 0) {
          facets = facets ::: List(Facet(name.text, name.text, None, n.text.toInt, applied.contains(name.text)))
        }
      })
    }
    FieldFacetClass(key, name, param, render, facets, sort)  
  }
}

case class QueryFacetClass(
  override val key: String,
  override val name: String,
  override val param: String,
  override val render: (String) => String = s=>s,
  facets: List[Facet] = Nil,
  override val sort: Enumeration = OrderedByName
) extends FacetClass(key,name,param,render,facets,sort) {
  override val fieldType: String = "facet.query"
  def asParams: List[facet.FacetParam] = {
    facets.map(p =>
      new facet.FacetParam(
        facet.Param(fieldType),
        facet.Value("%s:%s".format(key, p.solrVal))
      )
    )      
  }
  override def populateFromSolr(data: xml.Elem, current: Map[String,Seq[String]]): FacetClass = {
    val applied = current.getOrElse(param, Seq[String]())
    val popfacets = facets.flatMap(f => {
      var nameval = "%s:%s".format(key, f.solrVal)
      data.descendant.filter(n => (n \\ "@name").text == nameval).text match {
        case "" => Nil
        case v => List(
          Facet(f.solrVal, f.paramVal, f.humanVal, v.toInt,
            applied.contains(f.paramVal))
        )
      }
    })
    QueryFacetClass(key, name, param, render, popfacets, sort)
  }
}

/**
 * Encapsulates a single facet.
 *
 * @param solrVal   the value of this facet to Solr
 * @param paramVal  the value as a web parameter
 * @param humanVal  the human-readable value
 * @param count     the number of objects to which this facet applies
 * @param applied   whether or not this facet is activated in the response
 */
case class Facet(
  val solrVal: String,
  val paramVal: String,
  val humanVal: Option[String] = None,
  val count: Int = 0,
  val applied: Boolean = false
) {

}



object FacetData {
  // Temporary hard-coded facet definitions... this will
  // be loaded from a config file at some point
  val facets = Map(
    "collection" -> List(
      FieldFacetClass(
        key="publication_status",
        name="Status",
        param="pub",
        render=(s: String) => s match {
          case "0" => "Draft"
          case "1" => "Published"
          case _ => "Unknown"
        }
      ),
      FieldFacetClass(
        key="django_ct",
        name="Resource Type",
        param="res",
        render=(s: String) => s match {
          case "portal.repository" => "Repository"
          case "portal.authority" => "Authority"
          case "portal.collection" => "Collection"
          case _ => "Unknown"
        }
      ),
      QueryFacetClass(
        key="years",
        name="Date",
        param="date",
        facets=List(
          Facet("[* TO 1939]", "_1939", Some("Up to 1939")),
          Facet("[1940 TO 1941]", "1940-1941", Some("1940 to 1941")),
          Facet("1941", "1941", Some("1941")),
          Facet("1946", "1946", Some("1946"))
        )
      ),
      FieldFacetClass(
        key="tags_exact",
        name="Keywords",
        param="tag"
      )
    ),
    "repository" -> List(
      FieldFacetClass(
        key="django_ct",
        name="Resource Type",
        param="res",
        render=(s: String) => s match {
          case "portal.repository" => "Repository"
          case "portal.authority" => "Authority"
          case "portal.collection" => "Collection"
          case _ => "Unknown"
        }
      )
    )
  )

  def constrain(request: QueryRequest, rtype: String, appliedFacets: Map[String,Seq[String]]): Unit = {
    facets.get(rtype).map(flist => {
      request.setFacet(new facet.FacetParams(
        enabled=true, 
        params=flist.map(_.asParams).flatten
      ))

      // filter the results by applied facets
      // NB: Scalikesolr is a bit dim WRT filter queries: you can
      // apparently only have one. So instead of adding multiple
      // fq clauses, we need to join them all with '+'
      val fqstring = flist.map(fclass => {
        appliedFacets.get(fclass.param).map(paramVals =>
          fclass match {
            case fc: FieldFacetClass => {
              paramVals.map("%s:%s".format(fc.key, _))
            }
            case fc: QueryFacetClass => {
              fc.facets.flatMap(facet => {
                if (paramVals.contains(facet.paramVal)) {
                  List("%s:%s".format(fc.key, facet.solrVal))
                } else Nil
              })
            }
          }
        ).getOrElse(Nil)
      }).flatten.mkString(" +") // NB: Space before + is important
      request.setFilterQuery(FilterQuery(fqstring))
    })
  }

  def extract(response: QueryResponse, rtype: String, appliedFacets: Map[String,Seq[String]]): List[FacetClass] = {
    val rawData = xml.XML.loadString(response.rawBody)
    facets.get(rtype).map(flist => {
      flist.map(_.populateFromSolr(rawData, appliedFacets))
    }).getOrElse(Nil)
  }
}

object Description {
  def list(
    index: Option[String] = None,
    page: Int = 0,
    pageSize: Int = 20,
    orderBy: Int = 1,
    field: String = "",
    query: String = "",
    facets: Map[String, Seq[String]] = Map()
  
  ): Page[Description] = {
    val offset = page * pageSize

    val queryString = "%s:%s".format(
      if (field.trim == "") "*" else field,
      if (query.trim == "") "*" else query)

    val client = Solr.httpServer(new java.net.URL("http://localhost:8983/solr")).newClient
    val req = new QueryRequest(query=Query(queryString))
    req.setFacet(new facet.FacetParams(
      enabled=true, 
      params=List(new facet.FacetParam(facet.Param("facet.field"), facet.Value("django_ct")))
    ))
    req.setQueryParserType(QueryParserType("edismax"))

    // Facet the request accordingly
    index.map(FacetData.constrain(req, _, facets))

    // if we're using a specific index, constrain on that as well
    var squery = index match {
      case Some(rt) => req.setFilterQuery(
          FilterQuery(req.filterQuery.fq + " +django_ct:portal." + rt))
      case None => 
    }

    // Setup start and number of objects returned
    req.setStartRow(request.query.StartRow(offset))
    req.setMaximumRowsReturned(request.query.MaximumRowsReturned(pageSize))

    // run the query
    val response = client.doQuery(req)

    // extract the useful classes from the response
    val fclasses = index.map(FacetData.extract(response, _, facets)).getOrElse(Nil)

    // We only care about documents with the following content types,
    // so use a flatMap to extract them into the correct classes
    Page(response.response.documents.flatMap(d => {
      d.get("django_ct").toString match {
        case "portal.repository" => List(d.bind(classOf[Repository]))
        case "portal.collection" => List(d.bind(classOf[Collection]))
        case "portal.authority" => List(d.bind(classOf[Authority]))
        case _ => Nil
      }
    }), page, offset, response.response.numFound, fclasses)
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


