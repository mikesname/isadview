package neo4j.models

object Description {
  private def idMatch = "^.+/(\\d+)$".r
}


trait Description {
  def self: String
  lazy val id: Option[Long] = self match {
    case Description.idMatch(id) => Some(id.toLong)
    case _ => None
  }
}
