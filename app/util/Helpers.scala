package app.util

import java.util.Locale

object Helpers {
  
  def languageCodeToName(code: String)(implicit locale: Locale): String = {
    new Locale(code, "").getDisplayLanguage(locale) match {
      case d if !d.isEmpty => d
      case _ => code
    }
  }                      
}
