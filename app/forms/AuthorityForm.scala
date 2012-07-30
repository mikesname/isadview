package forms

import play.api.data._
import play.api.data.format._
import play.api.data.Forms._

import models._

object AuthorityForm {

  // FIXME: Work out a way to have forms bind and unbind enumeration values 
  // without all this guff below... there must be an easier way.
  // The parsing and stringFormat formatters are copy/pasted from the play 2.0 source

  implicit def atypeFormat: Formatter[AuthorityType.AuthorityType] = new Formatter[AuthorityType.AuthorityType]{

    implicit def stringFormat: Formatter[String] = new Formatter[String] {
      def bind(key: String, data: Map[String, String]) = data.get(key).toRight(Seq(FormError(key, "error.required", Nil)))
      def unbind(key: String, value: String) = Map(key -> value)
    }

    private def parsing[T](parse: String => T, errMsg: String, errArgs: Seq[Any])(key: String, data: Map[String, String]): Either[Seq[FormError], T] = {
        stringFormat.bind(key, data).right.flatMap { s =>
          util.control.Exception.allCatch[T]
            .either(parse(s))
            .left.map(e => Seq(FormError(key, errMsg, errArgs)))
        }
      }

    override val format = Some("format.numeric", Nil)
    def bind(key: String, data: Map[String,String]) = 
      parsing(s => AuthorityType(s.toInt), "error.number", Nil)(key, data)
    def unbind(key: String, value: AuthorityType.AuthorityType) = Map(key -> value.toString)
  }


  val form = Form(
    mapping(
      "identity" -> mapping(
        "typeOfEntity" -> of[AuthorityType.AuthorityType](atypeFormat),
        "identifier" -> nonEmptyText,
        "name" -> nonEmptyText,
        "otherFormsOfName" -> list(text)
      )(AuthorityIdentity.apply)(AuthorityIdentity.unapply),
      "description" -> mapping(
        "datesOfExistence" -> optional(text),
        "history" -> optional(text),
        "places" -> optional(text),
        "functions" -> optional(text),
        "geneology" -> optional(text),
        "generalContext" -> optional(text)
      )(AuthorityDetails.apply)(AuthorityDetails.unapply),
      "control" -> mapping(
        "descriptionIdentifier" -> optional(text),
        "institutionIdentifier" -> optional(text),
        "rules" -> optional(text),
        "status" -> optional(text),
        "levelOfDetail" -> optional(text),
        "datesOfCreationRevisionDeletion" -> optional(text),
        "languagesOfDescription" -> list(text),
        "scriptsOfDescription" -> list(text),
        "mainainenceNotes" -> optional(text),
        "sources" -> optional(text)
      )(AuthorityControl.apply)(AuthorityControl.unapply),
      "admin" -> mapping(
        "publicationStatus" -> number
      )(AuthorityAdmin.apply)(AuthorityAdmin.unapply)
    )(AuthorityDescription.apply)(AuthorityDescription.unapply)
  )
}
