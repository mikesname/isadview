package app.util

import java.util.Locale

object Helpers {
  // FIXME: Unfortunately JDK7 Locale lacks a getISOScripts function
  // Need to expand this list from a complete source.
  private val tmpScriptMap: Map[String,String] = Map(
    "Brai" -> "Braille",
    "Cyrl" -> "Cyrillic",
    "Goth" -> "Gothic",
    "Hebr" -> "Hebrew",
    "Latn" -> "Latin"
  )

  def localeFromRequest(request: play.api.mvc.RequestHeader): Locale = {
    request.acceptLanguages.headOption.map(_.toLocale).getOrElse(
      new Locale("en", "GB"))
  }

  def languageCodeToName(code: String)(implicit locale: Locale): String = {
    new Locale(code, "").getDisplayLanguage(locale) match {
      case d if !d.isEmpty => d
      case _ => code
    }
  }

  def scriptCodeToName(code: String)(implicit locale: Locale): String = {
    var tmploc = new Locale.Builder().setScript(code).build()
    tmploc.getDisplayScript(locale) match {
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

  def scriptList(implicit locale: Locale): List[(String,String)] = {
    tmpScriptMap.map { case (code, name) => (
      code, scriptCodeToName(code)(locale))
    }.toList
  }

  def countryList(implicit locale: Locale): List[(String,String)] = {
    Locale.getISOCountries.map(code => (
      code, new Locale(locale.getLanguage, code).getDisplayCountry(locale))).toList
  }
}

