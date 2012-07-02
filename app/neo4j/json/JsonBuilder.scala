package neo4j.json

import net.liftweb.json.JsonAST.JValue

import play.api.PlayException

case class GremlinError(
  val message: String, val exception: String, val stacktrace: List[String]
) extends PlayException(title="Gremlin Script Error: %s".format(exception), description=message)

trait JsonBuilder[T] {
  private val idMatch = "^.+/(\\d+)$".r
  def idFromUrl(url: Option[String]): Long = url match {
    case Some(idMatch(s)) => s.toLong
    case _ => -1
  }

  def apply(data: net.liftweb.json.JsonAST.JValue): T
  
  def list(data: JValue): List[T] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    // attempt to ensure we catch a known type of error that
    // comes back from Neo4j if it encounters an error in a
    // Gremlin script.
    data.extractOpt[GremlinError].map(throw _)
    data.children.map(apply(_))
  }
}

