
package app.forms

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import app.models.Description

object Forms {
    val descriptionForm = Form(
      mapping(
        "identifier" -> nonEmptyText,
        "slug" -> nonEmptyText,
        "name" -> nonEmptyText
      )(Description.apply)(Description.unapply)
    )
}
