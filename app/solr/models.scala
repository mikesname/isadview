package solr.models

import com.github.seratch.scalikesolr._
import com.github.seratch.scalikesolr.response.QueryResponse
import com.github.seratch.scalikesolr.request.query.{Query, FilterQuery, QueryParserType}
import com.github.seratch.scalikesolr.request.query.highlighting.{
    IsPhraseHighlighterEnabled, HighlightingParams}

import play.api.i18n

import solr.facet.{FacetData,FacetClass,FieldFacetClass,QueryFacetClass,Facet}
import com.github.seratch.scalikesolr.request.query.facet.{FacetParams,FacetParam,Param,Value}

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long, facets: List[FacetClass]) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

case class FacetPage(fc: FacetClass, facets: List[Facet], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + facets.size) < total)
}

object SolrHelper {

  private def setRequestFacets(request: QueryRequest, flist: List[FacetClass]): Unit = {
    request.setFacet(new FacetParams(
      enabled=true, 
      params=flist.map(_.asParams).flatten
    ))
  }

  private def setRequestFilters(request: QueryRequest, flist: List[FacetClass], applied: Map[String,Seq[String]]): Unit = {
    // filter the results by applied facets
    // NB: Scalikesolr is a bit dim WRT filter queries: you can
    // apparently only have one. So instead of adding multiple
    // fq clauses, we need to join them all with '+'
    val fqstring = flist.map(fclass => {
      applied.get(fclass.param).map(paramVals =>
        fclass match {
          case fc: FieldFacetClass => {
            paramVals.map("%s:\"%s\"".format(fc.key, _))
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
  }

  def constrain(request: QueryRequest, rtype: String, appliedFacets: Map[String,Seq[String]]): Unit = {
    FacetData.facets.get(rtype).map(flist => {
      setRequestFacets(request, flist)
      setRequestFilters(request, flist, appliedFacets)
    })
  }

  def extract(response: QueryResponse, rtype: String, appliedFacets: Map[String,Seq[String]]): List[FacetClass] = {
    val rawData = xml.XML.loadString(response.rawBody)
    FacetData.facets.get(rtype).map(flist => {
      flist.map(_.populateFromSolr(rawData, appliedFacets))
    }).getOrElse(Nil)
  }

  def buildQuery(index: Option[String], offset: Int, pageSize: Int, orderBy: Int,
        field: String, query: String, facets: Map[String, Seq[String]]): Tuple2[QueryResponse, List[FacetClass]] = {

    // Solr 3.6 seems to break querying with *:<query> style
    // http://bit.ly/MBKghG
    //val queryString = "%s:%s".format(
    //  if (field.trim == "") "*" else field,
    //  if (query.trim == "") "*" else query)
    val queryString = if (query.trim == "") "*" else query.trim

    val client = Solr.httpServer(new java.net.URL(play.Play.application.configuration.getString("solr.path"))).newClient
    val req = new QueryRequest(query=Query(queryString))
    req.setFacet(new FacetParams(
      enabled=true, 
      params=List(new FacetParam(Param("facet.field"), Value("django_ct")))
    ))
    req.setQueryParserType(QueryParserType("edismax"))
    req.setHighlighting(HighlightingParams(
        enabled=true,
        isPhraseHighlighterEnabled=IsPhraseHighlighterEnabled(true)))

    // Facet the request accordingly
    index.map(SolrHelper.constrain(req, _, facets))

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
    val fclasses = index.map(SolrHelper.extract(response, _, facets)).getOrElse(Nil)
    (response, fclasses)
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

    val(resp, fclasses) = SolrHelper.buildQuery(
          index, offset, pageSize, orderBy, field, query, facets)

    // We only care about documents with the following content types,
    // so use a flatMap to extract them into the correct classes
    Page(resp.response.documents.flatMap(d => {
      d.get("django_ct").toString match {
        case "portal.repository" => List(d.bind(classOf[Repository]))
        case "portal.collection" => List(d.bind(classOf[Collection]))
        case "portal.authority" => List(d.bind(classOf[Authority]))
        case _ => Nil
      }
    }), page, offset, resp.response.numFound, fclasses)
  }
  
  def facet(
    facet: String,
    index: Option[String] = None,
    page: Int = 0,
    pageSize: Int = 20,
    sort: String = "name",
    field: String = "",
    query: String = "",
    facets: Map[String, Seq[String]] = Map()
  
  ): FacetPage = {
    val offset = page * pageSize

    // create a response returning 0 documents - we don't
    // actually care about the documents, so even this is
    // not strictly necessary... we also don't care about the
    // ordering.
    val (resp, fclasses) = SolrHelper.buildQuery(
          index=index, offset=0, pageSize=0, orderBy=0,
          field=field, query=query, facets=facets)
    
    val fclass = fclasses.find(_.param==facet).getOrElse(
        throw new Exception("Unknown facet: " + facet))
    val flist = sort match {
      case "name" => fclass.sortedByName.slice(offset, offset + pageSize)
      case _ => fclass.sortedByCount.slice(offset, offset + pageSize)
    }
    FacetPage(fclass, flist, page, offset, fclass.count)
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


