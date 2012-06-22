package neo4j.models

import java.util.Locale

object Contact extends JsonBuilder[Contact] {
  implicit val formats = net.liftweb.json.DefaultFormats
  
  def apply(data: net.liftweb.json.JsonAST.JValue): Contact = {
    Contact(
      id = idFromUrl((data \ "self").extractOpt[String]),
      primary = (data \ "data" \ "primary").extractOpt[Boolean].getOrElse(false),
      contactPerson = (data \ "data" \ "contact_person").extractOpt[String],
      streetAddress = (data \ "data" \ "identifier").extractOpt[String],
      city = (data \ "data" \ "city").extractOpt[String],
      region = (data \ "data" \ "region").extractOpt[String],
      countryCode = (data \ "data" \ "country_code").extractOpt[String],
      postalCode = (data \ "data" \ "postal_code").extractOpt[String],
      telephone = (data \ "data" \ "telephone").extractOpt[String],
      fax = (data \ "data" \ "fax").extractOpt[String],
      email = (data \ "data" \ "email").extractOpt[String],
      website = (data \ "data" \ "website").extractOpt[String],
      note = (data \ "data" \ "note").extractOpt[String]
    )
  }
}

case class Contact(
  val primary: Boolean = false,
  val contactPerson: Option[String] = None,
  val streetAddress: Option[String] = None,
  val city: Option[String] = None,
  val region: Option[String] = None,
  val postalCode: Option[String] = None,
  val countryCode: Option[String] = None,
  val telephone: Option[String] = None,
  val fax: Option[String] = None,
  val email: Option[String] = None,
  val website: Option[String] = None,
  val note: Option[String] = None,
  val id: Long = -1
) extends Description {
  def countryName(loc: Locale): Option[String] = countryCode match {
    case Some(code) => Some(new Locale("", code).getDisplayCountry(loc))
    case _ => None
  }

  def format(loc: Locale = new Locale("en", "GB")): String = {
    List(streetAddress, city, region, postalCode, countryName(loc)).flatMap {
        case Some(addr) if addr.trim != "" => List(addr)
        case _ => Nil
    }.mkString("\n")
  }
}

