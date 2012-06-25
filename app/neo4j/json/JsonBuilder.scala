package neo4j.json

import net.liftweb.json.JsonAST.JValue

case class NoResultsFound(err: String = "") extends Exception
case class MultipleResultsFound(err: String = "") extends Exception


trait JsonBuilder[T] {
  private val idMatch = "^.+/(\\d+)$".r
  def idFromUrl(url: Option[String]): Long = url match {
    case Some(idMatch(s)) => s.toLong
    case _ => -1
  }

  def apply(data: net.liftweb.json.JsonAST.JValue): T
  
  def list(data: JValue): List[T] = {
    data.children.map(apply(_))
  }

  def one(data: JValue): T = {
    var l = list(data)
    if (l.length == 0) throw new NoResultsFound()
    else if (l.length > 1) throw new MultipleResultsFound()
    l.head
  }
}

