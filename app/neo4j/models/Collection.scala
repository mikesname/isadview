package neo4j.models

case class Collection(
  val self: String,
  val data: CollectionData
) extends Description {

}

case class CollectionData(
  val slug: String = "",
  val name: String = "",
  val identifier: String = "",
  val publication_status: Int = 0,
  val scope_and_content: Option[String] = None,
  val history: Option[String] = None,
  val arrangement: Option[String] = None,
  val extent_and_medium: Option[String] = None,
  val acquisition: Option[String] = None,
  val sources: Option[String] = None,
  val rules: Option[String] = None
)
{
  lazy val otherNames: List[String] = Nil
}

