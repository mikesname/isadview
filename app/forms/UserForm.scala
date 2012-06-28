package forms

import play.api.data._
import play.api.data.Forms._

import models._

object UserForm {
  val signupForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "email" -> email
    ) verifying ("Username has already been taken", result => result match {
      case (username, email) => User.checkUniqueUsername(username)
    })
  )

  val profile = Form(
    mapping(
      "name" -> optional(text),
      "languages" -> list(text)
    )(UserProfile.apply)(UserProfile.unapply)
  )
}
