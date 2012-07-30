package importers


trait XmlHelper {
  def optString(s: String) = if (s.trim.isEmpty) None else Some(s)
}
