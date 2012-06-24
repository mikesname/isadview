package app.util

import java.util.Locale

object Helpers {

  def languageCodeToName(code: String)(implicit locale: Locale): String = {
    new Locale(code, "").getDisplayLanguage(locale) match {
      case d if !d.isEmpty => d
      case _ => code
    }
  }

  def countryCodeToName(code: String)(implicit locale: Locale): String = {
    new Locale("", code).getDisplayCountry(locale) match {
      case d if !d.isEmpty => d
      case _ => code
    }
  }

  def languageList(implicit locale: Locale): List[(String,String)] = {
    Locale.getISOLanguages.map(code => (
      code, new Locale(code).getDisplayLanguage(locale))).toList
  }

  def countryList(implicit locale: Locale): List[(String,String)] = {
    Locale.getISOCountries.map(code => (
      code, new Locale(locale.getLanguage, code).getDisplayCountry(locale))).toList
  }
}
