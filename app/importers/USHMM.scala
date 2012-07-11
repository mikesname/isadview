package importers

import scala.xml._
import models._
import com.codahale.jerkson.Json.generate

/* Geoff extractor for USHMM Solr dumps, which look extactly
   like Solr 'add' documents. */

object USHMM {
  private val datePattern = ".+-- (\\d{4})-(\\d{4})\\.?$".r

  def optString(s: String) = if (s.trim.isEmpty) None else Some(s)

  def attributeValueEquals(value: String)(node: Node) = {
    node.attributes.exists(_.value.text == value)
  }

  def getField(field: String, elem: NodeSeq): Option[String] = {
    optString((elem \ "field").filter(attributeValueEquals(field)).text)
  }

  def multiFields(fields: List[String], sep: String, elem: NodeSeq) = {
    optString(fields.map { fname =>
      (elem \ "field").filter(attributeValueEquals(fname)).text
    }.filter(_.trim.nonEmpty).mkString("\n"))
  }

  /* Extract date fields, which in the USHMM dump format are
  the last YYYY-YYYY field on keyword access fields. */
  def extractDates(elem: NodeSeq): List[FuzzyDate] = {
    (elem \ "field").filter(attributeValueEquals("subject_heading")).flatMap { f =>
      f.text match {
        case datePattern(start, end) => List(FuzzyDate(start.toInt, end.toInt))
        case _ => Nil
      }
    }.toList
  }

  def extractKeyValues(ident: String, elem: NodeSeq) = {
   Map(
      "identifier"  -> ident,
      "element_type" -> Collection.indexName,
      "name"        -> getField("title", elem),
      "slug"        -> app.util.Helpers.slugify(
          List(getField("title", elem).getOrElse(""), ident).filter(_.nonEmpty).mkString(" ")),
      "source"  -> getField("acq_source", elem),
      "scope_and_content"  -> getField("scope_content", elem),
      "extent_and_medium"  -> getField("extent", elem),
      "legal_status"  -> getField("legal_status", elem),
      "created_on" -> Collection.nowDateTime,
      "acquisition"  -> multiFields(List("acq_source", "acccession_number", "acq_credit"), "\n", elem)
    )           
  }

  def docToGeoff(repoid: Long, elem: NodeSeq): List[String] = {
    getField("irn", elem).map { ident =>
      val data = extractKeyValues(ident, elem)
      val json = generate(data)
      val dates = extractDates(elem).zipWithIndex.flatMap { case(d, i) =>
        "(%sdate%d) %s".format(ident, i, generate(d.toMap)) ::
     //   "(%sdate%d)<=|%s| %s".format(ident, i, FuzzyDate.indexName, generate(d.toMap)) ::
        "(%sdate%d)-[%sdate%drel:locatesInTime]->(%s)".format(ident, i, ident, i, ident) :: Nil
      }
      val idxkeys = List("identifier", "name", "slug")
      val reporel = List(
        "(%s)-[%sparent:heldBy]->(repo%d)".format(ident, ident, repoid)
      )
      List(
        "(%s) %s".format(ident, json),
        "(%s)<=|%s| %s".format(ident, Collection.indexName, generate(data.filterKeys(k => idxkeys.contains(k))))
      ) ++ dates ++ reporel
    }.getOrElse(Nil)
  }
}
