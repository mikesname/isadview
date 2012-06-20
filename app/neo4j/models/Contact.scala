package neo4j.models

import java.util.Locale

case class Contact(
  val self:String = "",
  val data: ContactData
) extends Description {

}

case class ContactData(
  val primary: Boolean = false,
  val contact_person: Option[String] = None,
  val street_address: Option[String] = None,
  val city: Option[String] = None,
  val region: Option[String] = None,
  val postal_code: Option[String] = None,
  val country_code: Option[String] = None,
  val publication_status: Int = 0,
  val telephone: Option[String] = None,
  val fax: Option[String] = None,
  val email: Option[String] = None,
  val website: Option[String] = None,
  val note: Option[String] = None
) extends DescriptionData {
  def countryName(loc: Locale): Option[String] = country_code match {
    case Some(code) => Some(new Locale("", code).getDisplayCountry(loc))
    case _ => None
  }

  def format(loc: Locale = new Locale("en", "GB")): String = {
    List(street_address, city, region, postal_code, countryName(loc)).flatMap {
        case Some(addr) if addr.trim != "" => List(addr)
        case _ => Nil
    }.mkString("\n")
  }
}



// vim: set ts=4 sw=4 et:
