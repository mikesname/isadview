package views


import views.html.helper.FieldConstructor

import org.ocpsoft.pretty.time.PrettyTime

object Helpers {
  def prettyDate(d: org.joda.time.DateTime): String = {
    val p = new PrettyTime() // TODO: Locale awareness...
    p.format(d.toDate)
  }

  def prettyDate(d: Option[org.joda.time.DateTime]): String = {
    d match {
      case Some(dt) => prettyDate(dt)
      case None => ""
    }
  }
}
