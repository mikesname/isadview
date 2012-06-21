package neo4j.models


import org.joda.time.DateTime


object FuzzyDate extends JsonBuilder[FuzzyDate] {
  implicit val formats = net.liftweb.json.DefaultFormats
  
  def apply(data: net.liftweb.json.JsonAST.JValue): FuzzyDate = {
    FuzzyDate(
      url = (data \ "self").extractOpt[String],
      startDate = (data \ "data" \ "start_date").extractOpt[String].map(new DateTime(_)),
      endDate = (data \ "data" \ "end_date").extractOpt[String].map(new DateTime(_)),
      precision = (data \ "data" \ "precision").extractOpt[String],
      circa = (data \ "circa" \ "circa").extractOpt[Boolean].getOrElse(false)
    )
  }
}

case class FuzzyDate(
  val startDate: Option[DateTime] = None,
  val endDate: Option[DateTime] = None,
  val precision: Option[String] = None,
  val circa: Boolean = false,
  val url: Option[String] = None
) extends Description with IdFromUrl {
  override def toString: String = {
    """Print a sensible representation."""
    var out = ""
    if (circa) out = "c" + out
    out + List(startDate, endDate).flatMap {
        case Some(d) => List(d.year.getAsString)
        case None => Nil
    }.mkString("-")
  }
}




