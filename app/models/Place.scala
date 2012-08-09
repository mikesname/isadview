package models


object Place extends neo4j.DataSource[Place] {
  val indexName = "place"
  
  def apply(data: net.liftweb.json.JsonAST.JValue): Place = {
    Place(
      id = idFromUrl((data \ "self").extractOpt[String]),
      text = (data \ "data" \ "text").extractOpt[String].getOrElse(""),
      standard = (data \ "data" \ "standard").extractOpt[String],
      note = (data \ "data" \ "note").extractOpt[String]
    )
  }

  def apply(text: String) = new Place(-1, text)
}

case class Place(
  val id: Long = -1,
  val text: String,
  val standard: Option[String] = None,
  val note: Option[String] = None,
  val parent: Option[Place] = None
) extends neo4j.Neo4jModel {
  def toMap = Map(
    Place.TypeKey -> Place.indexName,
    "text" -> text,
    "standard" -> standard,
    "note" -> note
  )

  override def toString = "<Place: %s>".format(text)
}

