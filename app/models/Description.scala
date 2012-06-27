package models

import org.joda.time.DateTime

trait CrudUrls extends Description {
  def detailUrl: play.api.mvc.Call
  def editUrl: play.api.mvc.Call
  def deleteUrl: play.api.mvc.Call
}

trait Description {
  def name: String
  def createdOn: Option[DateTime]
  def updatedOn: Option[DateTime]
}
