package models

import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.ISODateTimeFormat

trait Entity {
  def id: Long
  def slug: Option[String]
  def name: String
}

trait CrudUrls extends Entity {
  def detailUrl: play.api.mvc.Call
  def editUrl: play.api.mvc.Call
  def deleteUrl: play.api.mvc.Call
}

trait CreatedUpdated {
  def createdOn: Option[DateTime]
  def updatedOn: Option[DateTime]
  def publicationDate = if (updatedOn.isDefined) updatedOn else createdOn
  def formatDate(d: org.joda.time.DateTime) = ISODateTimeFormat.dateTime.print(d)
  def formatSolrDate(d: org.joda.time.DateTime) = ISODateTimeFormat.dateTime.withZone(DateTimeZone.UTC).print(d)
}

trait Stashable extends Entity {
  def summary: Option[String]
  def details: Map[String, Any]
}

trait Description extends CrudUrls with CreatedUpdated with Stashable


