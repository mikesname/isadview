package views


import views.html.helper.FieldConstructor

import org.ocpsoft.pretty.time.PrettyTime


// Pimp my 
package object Helpers {

  // These classes follow the "pimp my library" pattern, adding
  // implicit conversions to types one views.Helpers._ is imported
  // into the current scope...
  class PimpedInt(int: Int) {
    def pluralize = if (int == 1) "" else "s"
  }
  class PimpedLong(long: Long) {
    def pluralize = if (long == 1L) "" else "s"
  }
  class PimpedTraversable[A](col: Traversable[A]) {
    def pluralize = if (col.size == 1) "" else "s"
  }

  implicit def pimpInt(int: Int) = new PimpedInt(int)
  implicit def pimpLong(long: Long) = new PimpedLong(long)
  implicit def pimpTraversable[A](col: Traversable[A]) = new PimpedTraversable(col)


  def prettyDate(d: java.util.Date): String = {
    val p = new PrettyTime() // TODO: Locale awareness...
    p.format(d)
  }
  def prettyDate(d: org.joda.time.DateTime): String = prettyDate(d.toDate)
  def prettyDate(d: Option[org.joda.time.DateTime]): String = {
    d match {
      case Some(dt) => prettyDate(dt.toDate)
      case None => ""
    }
  }

  def paginationArray(lastPage: Int, maxNums: Int, current: Int, window: Int = 3): List[Range] = {
    lastPage match {
      case x if x < maxNums => List(1 to lastPage)
      case x => {
        // This is WIP and doesn't currently work...
        val full = (1 to lastPage)
        val before = full.takeWhile(_ < (current - window))
        val after = full.dropWhile(_ > (current + window))
        val middle = full.takeWhile(_ < (current - window)).dropWhile(_ > (current + window))
        List(before, middle, after)
      }
    }
  }
}
