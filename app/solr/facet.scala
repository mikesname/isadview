package solr.facet

import java.util.Locale

import app.util.Helpers

import com.github.seratch.scalikesolr.request.query.facet.{FacetParams, FacetParam, Param, Value}

import models.AuthorityType


object Utils {

  def joinPath(path: String, qs: Map[String, Seq[String]]): String = {
    List(path, joinQueryString(qs)).filterNot(_=="").mkString("?")
  }

  def joinQueryString(qs: Map[String, Seq[String]]): String = {
    import java.net.URLEncoder
    qs.map { case (key, vals) => {
      vals.map(v => "%s=%s".format(key, URLEncoder.encode(v, "UTF-8")))    
    }}.flatten.mkString("&")
  }

  def pathWithoutFacet(fc: FacetClass, f: Facet, path: String, qs: Map[String, Seq[String]]): String = {
    joinPath(path, qs.map(qv => {
      qv._1 match {
        case fc.param => (qv._1, qv._2.filter(_!=f.paramVal))
        case _ => qv
      }
    }))
  }

  def pathWithFacet(fc: FacetClass, f: Facet, path: String, qs: Map[String, Seq[String]]): String = {
    joinPath(path, if (qs.contains(fc.param)) {
        qs.map(qv => {
          qv._1 match {
            case fc.param => (qv._1, qv._2.union(Seq(f.paramVal)).distinct)
            case _ => qv
          }
        })
      } else qs.updated(fc.param, Seq(f.paramVal))
    )
  }
}

// Scala's enum-like Phantom types for defining
// how facets are ordered
case object NameOrder extends Enumeration
case object CountOrder extends Enumeration
case object FixedOrder extends Enumeration

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
  def sortVal = humanVal.getOrElse(paramVal)
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
 * @param facets  a list of individual Facet values
 */
sealed abstract class FacetClass (
  val key: String,
  val name: String,
  val param: String,
  val render: (String) => String = s => s,
  private val facets: List[Facet] = Nil,
  val sort: Enumeration = CountOrder
)
{
  val fieldType: String
  
  def count: Int = facets.length
  
  def filtered: List[Facet] = facets.filter(_.count > 0)
  
  def sortedByName = filtered.sortWith((a, b) => a.sortVal < b.sortVal)

  def sortedByCount = filtered.sortWith((a, b) => b.count < a.count)

  def sorted: List[Facet] = sort match {
    case NameOrder => sortedByName
    case CountOrder => sortedByCount
    case _ => filtered
  }
  
  def asParams: List[FacetParam]
  
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
  override val sort: Enumeration = CountOrder
) extends FacetClass(key,name, param,render,facets,sort) {
  override val fieldType: String = "facet.field"
  
  def asParams: List[FacetParam] = {
    List(new FacetParam(
      Param(fieldType),
      Value(key)
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
  override val sort: Enumeration = NameOrder
) extends FacetClass(key,name,param,render,facets,sort) {
  override val fieldType: String = "facet.query"
  
  def asParams: List[FacetParam] = {
    facets.map(p =>
      new FacetParam(
        Param(fieldType),
        Value("%s:%s".format(key, p.solrVal))
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


object FacetData {
  // Temporary hard-coded facet definitions... this will
  // be loaded from a config file at some point
  
  // FIXME: Work out out to get the preferred lang
  // from the application context somehow
  implicit val locale: Locale = Locale.getDefault

  val facets = Map(
    "all" -> List(
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
    ),
    models.Collection.indexName -> List(
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
      QueryFacetClass(
        key="years",
        name="Date",
        param="date",
        facets=List(
          Facet("[* TO 1933]", "_1933", Some("Up to 1933")),
          Facet("[1933 TO 1939]", "1933_1939", Some("1933 to 1939")),
          Facet("1939", "1939", Some("1939")),
          Facet("1940", "1940", Some("1940")),
          Facet("1941", "1941", Some("1941")),
          Facet("1942", "1942", Some("1942")),
          Facet("1943", "1943", Some("1943")),
          Facet("1944", "1944", Some("1944")),
          Facet("1945", "1945", Some("1945")),
          Facet("[1946 TO *]", "1946_", Some("1946 Onwards"))
        ),
        sort=FixedOrder
      ),
      FieldFacetClass(
        key="languages",
        name="Languages of Materials",
        param="lang",
        render=Helpers.languageCodeToName
      ),
      FieldFacetClass(
        key="tags_exact",
        name="Keywords",
        param="tag"
      )
    ),
    models.Authority.indexName -> List(
      FieldFacetClass(
        key="type_of_entity",
        name="Type",
        param="type",
        render=(s: String) => AuthorityType(s.toInt).toString  
      )
    ),
    models.Repository.indexName -> List(
      FieldFacetClass(
        key="country",
        name="Location of Materials",
        param="loc",
        render=Helpers.countryCodeToName
      )
    )
  )

  def getForIndex(rtype: Option[String]) = {
    rtype match {
      case None => facets.flatMap { case(k, l) => l }.toList
      case Some(index) => facets.get(index).getOrElse(Nil)
    }
  }
}

