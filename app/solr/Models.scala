package solr.models

import com.github.seratch.scalikesolr._
import com.github.seratch.scalikesolr.response.QueryResponse
import com.github.seratch.scalikesolr.request.query.{Query, FilterQuery}
import com.github.seratch.scalikesolr.request.query.facet

import play.api.i18n

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long, facets: List[FClass]) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

trait FClass {
  val key: String
  val name: String
  val param: String
  val fieldType: String
  val render: (String) => String
  def facets: List[Facet] = Nil
  def filtered: List[Facet] = facets.filter(_.count > 0)
  def sortedByCount: List[Facet] = {
    filtered.sortWith((a, b) => a.count < b.count)
  }
  def addFacet(f: Facet): FClass
  def sortedByName: List[Facet] = {
    filtered.sortWith((a, b) => a.pretty < b.pretty)
  }
  def asParams: List[facet.FacetParam]
}

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
 * @oaram facets  a list of individual Facet values
 */

case class FacetClass(
  override val key: String,
  override val name: String,
  override val param: String,
  override val render: (String) => String = s=>s,
  override val facets: List[Facet] = Nil
) extends FClass {
  override val fieldType: String = "facet.field"
  def addFacet(f: Facet) = {
    this.copy(facets = facets ::: List(f))
  }
  def asParams: List[facet.FacetParam] = {
    List(new facet.FacetParam(
      facet.Param(fieldType),
      facet.Value(key)
    ))      
  }
}

case class QueryFacetClass(
  override val key: String,
  override val name: String,
  override val param: String,
  override val render: (String) => String = s=>s,
  override val facets: List[Facet] = Nil,
  val points: List[String] = Nil
) extends FClass {
  override val fieldType: String = "facet.query"
  def addFacet(f: Facet) = {
    this.copy(facets = facets ::: List(f))
  }
  def asParams: List[facet.FacetParam] = {
    points.map(p =>
      new facet.FacetParam(
        facet.Param(fieldType),
        facet.Value("%s:%s".format(key, p))
      )
    )      
  }
}



trait Facet {
  val klass: FClass
  val value: String
  val count: Long
  val applied: Boolean
  val desc: Option[String]
  
  def pretty: String = desc match {
    case Some(value) => klass.render(value)
    case None => klass.render(value)
  }
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
case class FieldFacet(
  override val klass: FClass,
  override val value: String, 
  override val count: Long,
  override val applied: Boolean = false,
  override val desc: Option[String] = None
) extends Facet {
}

case class QueryFacet(
  override val klass: FClass,
  override val value: String, 
  override val count: Long,
  override val applied: Boolean = false,
  override val desc: Option[String] = None
) extends Facet {

}

case class DateFacet(
  override val klass: FClass,
  override val value: String, 
  override val count: Long,
  override val applied: Boolean = false,
  override val desc: Option[String] = None
) extends Facet {

}



object FacetData {
  val facets = Map(
    "collection" -> List(
      FacetClass(
        "publication_status",
        "Status",
        "pub",
        (s: String) => s match {
          case "0" => "Draft"
          case "1" => "Published"
          case _ => "Unknown"
        }
      ),
      FacetClass(
        "django_ct",
        "Resource Type",
        "res"
      ),
      QueryFacetClass(
        "years",
        "Date",
        "date",
        points=List(
          "[* TO 1939]",
          "[1939 TO 1940]"
        )
      )
    ),
    "repository" -> List(
      FacetClass(
        "django_ct",
        "Resource Type",
        "res"
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
        appliedFacets.get(fclass.param).map(st =>
            st.map("%s:%s".format(fclass.key, _))
        ).getOrElse(Nil)
      }).flatten.mkString(" +") // NB: Space before + is important
      request.setFilterQuery(FilterQuery(fqstring))
    })
  }

  def extractFrom[F <: Facet](keydocs: Map[String, SolrDocument], flist: List[FClass],
      appliedFacets: Map[String,Seq[String]], factory: (FClass, String, Long, Boolean, Option[String] 
        ) => F): List[FClass] = {
    // TODO: Find a less horrible way of doing this...
    var fclasses = List[FClass]()
    keydocs.foreach { case (key, solrdoc) => {
      flist.find(_.key == key.split(":").head).map(fc => {
        var ffc = fc
        solrdoc.getMap.foreach { case (fval, num) => {
          val applied = appliedFacets.get(fc.param).map(_.map(_.replace(fc.key + ":", "")).contains(fval)).getOrElse(false)
          ffc = ffc.addFacet(
            factory(fc, fval, num.toLongOrElse(0), applied, None)
          )
        }}
        fclasses = fclasses ::: List(ffc)
      })
    }}
    fclasses
  }

  def extract(response: QueryResponse, rtype: String, appliedFacets: Map[String,Seq[String]]): List[FClass] = {
    facets.get(rtype).map(flist => {
      extractFrom(response.facet.facetFields, flist, appliedFacets, FieldFacet.apply) ++
      extractFrom(response.facet.facetQueries, flist, appliedFacets, QueryFacet.apply)
    }).getOrElse(Nil)
  }
}

object Description {
  def list(
    index: Option[String] = None,
    page: Int = 0,
    pageSize: Int = 20,
    orderBy: Int = 1,
    query: String = "",
    facets: Map[String, Seq[String]] = Map()
  
  ): Page[Description] = {
    val offset = page * pageSize
    var squery = index match {
      case Some(x) => "django_ct:portal." + x
      case None => "*:*"
    }

    val client = Solr.httpServer(new java.net.URL("http://localhost:8983/solr")).newClient
    val req = new QueryRequest(query=Query(squery))
    req.setFacet(new facet.FacetParams(
      enabled=true, 
      params=List(new facet.FacetParam(facet.Param("facet.field"), facet.Value("django_ct")))
    ))

    // Facet the request accordingly
    index.map(FacetData.constrain(req, _, facets))

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


