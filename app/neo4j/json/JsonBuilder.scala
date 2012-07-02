package neo4j.json

import net.liftweb.json.JsonAST.JValue


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
}

