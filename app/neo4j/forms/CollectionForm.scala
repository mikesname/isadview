package neo4j.forms

//case class Collection(
//  val self: String,
//  val data: CollectionData
//) extends Description {
//
//}
//
//case class CollectionData(
//  val slug: String = "",
//  val name: String = "",
//  val identifier: String = "",
//  val publication_status: Int = 0,
//  val scope_and_content: Option[String] = None,
//  val history: Option[String] = None,
//  val arrangement: Option[String] = None,
//  val extent_and_medium: Option[String] = None,
//  val acquisition: Option[String] = None,
//  val sources: Option[String] = None,
//  val rules: Option[String] = None
//)
//{
//  lazy val otherNames: List[String] = Nil
//  lazy val languages: List[String] = Nil
//  lazy val scripts: List[String] = Nil
//}
//


import play.api.data._
import play.api.data.Forms._

import neo4j.models.{Collection,CollectionData}

object CollectionForm {

  val form = Form(
    mapping(
      "slug" -> nonEmptyText,
      "name" -> nonEmptyText,
      "identifier" -> nonEmptyText,
      "publicationStatus" -> number,
      "scope_and_content" -> optional(text),
      "history" -> optional(text), 
      "arrangement" -> optional(text),  
      "extent_and_medium" -> optional(text),  
      "acquisition" -> optional(text),  
      "sources" -> optional(text),  
      "rules" -> optional(text)  
    )(CollectionData.apply)(CollectionData.unapply)
  )
}
