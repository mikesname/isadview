package neo4j.models

import neo4j.models.Description

case class Repository(
  val self: String,
  val data: RepositoryData
) extends Description {

}

case class RepositoryData(
  val slug: String = "",
  val name: String = "",
  val identifier: String = "",
  val publication_status: Int = 0,
  val geocultural_context: Option[String] = None,
  val history: Option[String] = None,
  val date: Option[java.util.Date] = None
) extends DescriptionData {
  lazy val otherNames: List[String] = Nil
}
