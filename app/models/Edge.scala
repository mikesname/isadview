package models

import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import neo4j.json.JsonBuilder
import neo4j.GremlinHelper

object Edge extends JsonBuilder[Edge] with GremlinHelper {
  def apply(data: net.liftweb.json.JsonAST.JValue): Edge = {
    Edge(
      id = idFromUrl((data \ "self").extractOpt[String]),
      start = idFromUrl((data \ "start").extractOpt[String]),
      end = idFromUrl((data \ "end").extractOpt[String]),
      label = (data \ "type").extractOpt[String],
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      description = (data \ "data" \ "description").extractOpt[String]
    )
  }
}

case class Edge(
  val id: Long = -1,
  val start: Long = -1,
  val end: Long = -1,
  val label: Option[String] = None,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val description: Option[String] = None
) {  
    override def toString = "<Edge: %d -> %s -> %d>".format(start,label,end)
}



