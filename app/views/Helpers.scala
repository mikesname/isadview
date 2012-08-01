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

  /*
   * Helper to provide Digg-style pagination, like:
   *    1, 2 ... 18, 19, 20, 21, 22 ... 55, 56
   * Logic borrowed from here:
   *   http://www.strangerstudios.com/sandbox/pagination/diggstyle_code.txt
   */
  def paginationRanges(page: Int, lastPage: Int, adjacents: Int = 3): List[Range] = {
    val window = adjacents * 2
    lastPage match {
      // Last page is the same as single page... no ranges
      case lp if lp <= 1 => Nil
      // Not enough pages to bother hiding any...
      case lp if lp < 7 + window =>  
        List((1 to lp))
      // Close to start, so only hide later pages
      case lp if lp > 5 + window && page < 1 + window =>
        List(1 until (4 + window), ((lp - 1) to lp))  
      // Around the middle, hide both start and end pages
      case lp if lp - window > page && page > window =>
        List((1 to 2), ((page - adjacents) to (page + adjacents)), ((lp - 1) to lp))
      // Close to end, hide beginning pages...
      case lp =>
        List((1 to 2), ((lp - (2 + window)) to lp))
    }
  }
}
