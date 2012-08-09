package models

import java.util.Locale

object Contact extends neo4j.DataSource[Contact] {
  val indexName = "contact"
  
  def apply(data: net.liftweb.json.JsonAST.JValue): Contact = {
    Contact(
      id = idFromUrl((data \ "self").extractOpt[String]),
      description = ContactDescription(
        primary = (data \ "data" \ "primary").extractOpt[Boolean].getOrElse(false),
        contactPerson = (data \ "data" \ "contact_person").extractOpt[String],
        streetAddress = (data \ "data" \ "street_address").extractOpt[String],
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
    )
  }
}

case class Contact(
  val id: Long = -1,
  val description: ContactDescription
) extends neo4j.Model {
  def toMap = Map(
    Contact.TypeKey -> Contact.indexName
  ) ++ description.toMap
}

case class ContactDescription(
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
  val note: Option[String] = None
) {
  def toMap = Map(
    "primary" -> primary,
    "contact_person" -> contactPerson,
    "street_address" -> streetAddress,
    "city" -> city,
    "region" -> region,
    "postal_code" -> postalCode,
    "country_code" -> countryCode,
    "telephone" -> telephone,
    "fax" -> fax,
    "email" -> email,
    "website" -> website,
    "note" -> note
  )
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

