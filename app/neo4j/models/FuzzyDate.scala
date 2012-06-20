package neo4j.models


import org.joda.time.DateTime


case class FuzzyDate(
  val self:String = "",
  val data: FuzzyDateData
) extends Description {

}

case class FuzzyDateData(
    val start_date: Option[String] = None,
    val end_date: Option[String] = None,
    val precision: Option[String] = None,
    val circa: Boolean = false
) {

  lazy val startDate: Option[DateTime] = start_date match {
    case Some(str) => Some(new DateTime(str))
    case None => None
  }

  lazy val endDate: Option[DateTime] = end_date match {
    case Some(str) => Some(new DateTime(str))
    case None => None
  }

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



