package models

import neo4j.data._

object Keyword extends Neo4jDataSource[Keyword] {
  val indexName = "keyword"
  
  def apply(data: net.liftweb.json.JsonAST.JValue): Keyword = {
    Keyword(
      id = idFromUrl((data \ "self").extractOpt[String]),
      text = (data \ "data" \ "text").extractOpt[String].getOrElse(""),
      standard = (data \ "data" \ "standard").extractOpt[String],
      note = (data \ "data" \ "note").extractOpt[String]
    )
  }

  def apply(text: String) = new Keyword(-1, text)
}

case class Keyword(
  val id: Long = -1,
  val text: String,
  val standard: Option[String] = None,
  val note: Option[String] = None,
  val parent: Option[Keyword] = None
) extends Neo4jModel {
  def toMap = Map(
    Keyword.TypeKey -> Keyword.indexName,
    "text" -> text,
    "standard" -> standard,
    "note" -> note
  )

  override def toString = "<Keyword: %s>".format(text)
}

