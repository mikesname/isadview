
package app.forms

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import app.models.Neo4jDescription

object Forms {
  val descriptionForm = Form(
    mapping(
      "identifier" -> nonEmptyText,
      "slug" -> nonEmptyText,
      "name" -> nonEmptyText,
      "otherNames" -> list(text)
    )(
      (identifier, slug, name, otherNames) => Neo4jDescription(None, identifier, slug, name, otherNames))(
      (d: Neo4jDescription) => Some((d.identifier, d.slug, d.name, d.otherNames))
    )
  )
}
