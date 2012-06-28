package forms

import play.api.data._
import play.api.data.Forms._

import models._

object UserForm {

  val loginForm = Form(
    tuple(
      "username" -> text,
      "email" -> email,
      "password" -> nonEmptyText
    ) verifying ("Invalid email or password", result => result match {
      case (username, email, password) => User.authenticate(email, password).isDefined
    })
  )

  val signupForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "email" -> email,
      "confirmEmail" -> email,
      "password" -> nonEmptyText,
      "confirmPassword" -> nonEmptyText
    ) verifying ("Passwords do not match", result => result match {
      case (u, e1, e2, p1, p2) => e1 == e2
    }) verifying ("Emails do not match", result => result match {
      case (u, e1, e2, p1, p2) => p1 == p2
    }) verifying ("Username has already been taken", result => result match {
      case (username, email, e2, password, p2) => User.checkUniqueUsername(username)
    })
  )

  val profile = Form(
    mapping(
      "name" -> optional(text),
      "languages" -> list(text)
    )(UserProfile.apply)(UserProfile.unapply)
  )
}
