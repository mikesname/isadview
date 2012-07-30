package models

import neo4j.data._
import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

/*

  Note: java.util.Date is awkward to work with so
  this class uses JodaTime's DateTime instead and
  does conversions to and from the form.
*/

object FuzzyDate extends Neo4jDataSource[FuzzyDate] {
  val indexName = "fuzzydate"

  def apply(data: net.liftweb.json.JsonAST.JValue): FuzzyDate = {
    FuzzyDate(
      id = idFromUrl((data \ "self").extractOpt[String]),
      description = FuzzyDateDescription(
        startDate = (data \ "data" \ "start_date").extractOpt[String].map(new DateTime(_)),
        endDate = (data \ "data" \ "end_date").extractOpt[String].map(new DateTime(_)),
        precision = (data \ "data" \ "precision").extractOpt[String],
        circa = (data \ "circa" \ "circa").extractOpt[Boolean]
      )
    )
  }


  // Another factory for FuzzyDate, this time created using
  // two integers representing start and end years
  def apply(start: Int, end: Int, precision: Option[String], circa: Option[Boolean]) = new FuzzyDate(
    -1L, new FuzzyDateDescription(
      startDate = Some(new DateTime(start, 1, 1, 0, 0, 0, 0)),
      endDate = Some(new DateTime(end, 12, 31, 0, 0, 0, 0)),
      precision = precision,
      circa = circa
    )
  )
  def apply(start: Int, end: Int): FuzzyDate = apply(start, end, Some("year"), Some(false))
  def apply(start: Int): FuzzyDate = new FuzzyDate(
    -1L, new FuzzyDateDescription(
      startDate = Some(new DateTime(start, 1, 1, 0, 0, 0, 0))
    )
  )
}

case class FuzzyDate(
  val id: Long = -1,
  val description: FuzzyDateDescription
) extends Neo4jModel {
  def toMap = Map(
    FuzzyDate.TypeKey -> FuzzyDate.indexName
  ) ++ description.toMap
}

case class FuzzyDateDescription(
  val startDate: Option[DateTime],
  val endDate: Option[DateTime] = None,
  val precision: Option[String] = None,
  val circa: Option[Boolean] = Some(false)
) {
  def toMap = Map(
    "start_date" -> startDate.map(ISODateTimeFormat.dateTime.print(_)),
    "end_date" -> endDate.map(ISODateTimeFormat.dateTime.print(_)),
    "circa" -> circa,
    "precision" -> precision
  )

  override def toString: String = {
    """Print a sensible representation."""
    var out = ""
    if (circa.getOrElse(false)) out = "c" + out
    out + List(startDate, endDate).flatMap {
        case Some(d) => List(d.year.getAsString)
        case None => Nil
    }.mkString("-")
  }
}


