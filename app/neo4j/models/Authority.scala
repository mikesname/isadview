package neo4j.models

case class Authority(
  val self: String,
  val data: AuthorityData
) extends Description {

}

case class AuthorityData(
  val slug: String = "",
  val name: String = "",
  val identifier: String = "",
  val publication_status: Int = 0,
  val dates_of_existence: Option[String] = None,
  val history: Option[String] = None
)
{
  lazy val otherNames: List[String] = Nil
}
