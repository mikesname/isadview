package neo4j.models

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
      startDate = (data \ "data" \ "start_date").extractOpt[String].map(new DateTime(_)),
      endDate = (data \ "data" \ "end_date").extractOpt[String].map(new DateTime(_)),
      precision = (data \ "data" \ "precision").extractOpt[String],
      circa = (data \ "circa" \ "circa").extractOpt[Boolean].getOrElse(false)
    )
  }

  def formApply(start: Option[Date], end: Option[Date]): FuzzyDate = {
    FuzzyDate(startDate = start.map(new DateTime(_)), endDate = end.map(new DateTime(_)))
  }

  def formUnapply(d: FuzzyDate): Option[(Option[Date], Option[Date])] = {
    Some((d.startDate.map(_.toDate), d.endDate.map(_.toDate)))
  }
}

case class FuzzyDate(
  val startDate: Option[DateTime],
  val endDate: Option[DateTime] = None,
  val precision: Option[String] = None,
  val circa: Boolean = false,
  val id: Long = -1
) extends Description {
  override def toString: String = {
    """Print a sensible representation."""
    var out = ""
    if (circa) out = "c" + out
    out + List(startDate, endDate).flatMap {
        case Some(d) => List(d.year.getAsString)
        case None => Nil
    }.mkString("-")
  }
  def toMap = Map(
    "start_date" -> startDate.map(ISODateTimeFormat.dateTime.print(_)),
    "end_date" -> endDate.map(ISODateTimeFormat.dateTime.print(_)),
    "circa" -> circa,
    "precision" -> precision
  )
}




