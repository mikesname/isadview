package forms

import play.api.data._
import play.api.data.Forms._

import models._

object UserForm {

  val openid = Form(single(
    "openid_identifier" -> nonEmptyText
  )) 

  val signupForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "email" -> email
    ) verifying ("Username has already been taken", result => result match {
      case (username, email) => models.sql.User.checkUniqueUsername(username)
    })
    )

  val profileForm = Form(
    mapping(
      "name" -> optional(text),
      "about" -> optional(text),
      "location" -> optional(text),
      "website" -> optional(text),
      "languages" -> list(text)
    )(ProfileData.apply)(ProfileData.unapply)
  )

  val virtualCollection = Form(
    tuple(
      "name" -> text,
      "description" -> optional(text)
    )
  )
}
