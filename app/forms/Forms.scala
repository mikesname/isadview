
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
      "name" -> nonEmptyText,
      "otherNames" -> list(text)
    )(
      (identifier, slug, name, otherNames) => Description(None, identifier, slug, name, otherNames))(
      (d: Description) => Some((d.identifier, d.slug, d.name, d.otherNames))
    )
  )
}
