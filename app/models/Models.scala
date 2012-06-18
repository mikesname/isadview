package app.models

import org.scala_tools.time.Imports.DateTime

import org.neo4j.graphdb._
import org.neo4j.rest.graphdb.RestGraphDatabase
import org.neo4j.scala._

object InstitutionType extends Enumeration(
    "International", "National", "Regional", "Community") {
  type InstitutionType = Value
  val International, National, Regional, Community = Value
}

object LevelOfDetail extends Enumeration(
    "Minimal", "Partial", "Complete") {
  type LevelOfDetail = Value
  val Minumum, Partial, Complete = Value
}

trait ContactType
// TODO: Different types of contact


object Description extends Neo4jWrapper with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
  var descIndex = "descriptions"
  var lookupProp = "slug"
  private val ARR_SEP = ",,"
  override def NodeIndexConfig = (descIndex, Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: Nil
  override def uri = new java.net.URI(play.Play.application.configuration.getString("neo4j.path"))
  override def userPw: Option[(String, String)] = None

  // deconstruct a case class to a Map
  private def getCCParams(cc: AnyRef) =
    (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(cc))
  }

  // fetch
  def getNode(slug: String): Option[Node] = {
    withTx { implicit ds =>
      try {
        Some(ds.gds.index().forNodes(descIndex).query(lookupProp, slug).getSingle())
      } catch {
        case _ => None
      }
    }           
  }

  def get(slug: String): Option[Description] = {
    getNode(slug) match {
      case Some(n) => Some(fromNode(n))
      case None => None
    }
  }
  
  def fromNode(node: Node): Description = {
    new Description(
      id=Some(node.getId()),
      identifier=node("identifier").getOrElse(""),
      slug=node("slug").getOrElse(""),
      name=node("name").getOrElse(""),
      otherNames=node("otherNames").getOrElse("").split(ARR_SEP).toList.filterNot(_=="")
    )
  }

  def save(d: Description): Unit = {
    withTx { implicit ds =>
      val node = d.id match {
        case Some(v) => getNode(d.slug).getOrElse(throw new Exception("Node not found with slug: %s".format(d.slug)))
        case None => createNode
      }
      val nIndex = getNodeIndex(descIndex).get
      for ((key, value) <- getCCParams(d)) {
      value match {
          case s: String => node.setProperty(key, s)
          case i: Number => node.setProperty(key, i)
          case null =>
          case None =>
          case other => {
            println("Saving array: %s".format(other))
            //node.setProperty(key, other.asInstanceOf[Array[String]])
            node.setProperty(key, other.asInstanceOf[List[String]].filterNot(_=="").mkString(ARR_SEP)) 
          }
        }
        nIndex += (node, key, value.toString)
      }
      d.id = Some(node.getId())
    }
    Unit
  }
}

case class Description(
  var id: Option[Long] = None,
  var identifier: String = "",
  var slug: String = "",
  var name: String = "",
  var otherNames: List[String] = Nil
) {
  def save: Unit = Description.save(this)
} 

//case class Contact(
//    primary: Boolean = true,
//    person: Option[String],
//    street_address: Option[String],
//    city: Option[String],
//    region: Option[String],
//    postal_code: Option[String],
//    country_code: Option[String],
//    website: Option[String],
//    email: Option[String],
//    telephone: Option[String],
//    fax: Option[String],
//    contact: Option[ContactType],
//    note: Option[String]) extends Description {
//
//}
//
//class Repository (
//    identifier: String,
//    name: String,
//    otherFormsOfName: List[String] = Nil,
//    parallelFormsOfName: List[String] = Nil,
//    typeOfInstitution: Option[InstitutionType.Value] = None,
//    contacts: List[Contact] = Nil,
//    history: Option[String] = None,
//    geographicalContext: Option[String] = None,
//    mandates: Option[String] = None,
//    administrativeStructure: Option[String] = None,
//    collectingPolicies: Option[String] = None,
//    buildings: Option[String] = None,
//    holdings: Option[String] = None,
//    findingAids: Option[String] = None,
//    openingTimes: Option[String] = None,
//    conditionsOfAccess: Option[String] = None,
//    accessibility: Option[String] = None,
//    researchServices: Option[String] = None,
//    reproductionServices: Option[String] = None,
//    publicAreas: Option[String] = None,
//    descriptionIdentifier: Option[String] = None,
//    institutionIdentifier: Option[String] = None,
//    rulesAndConventions: Option[String] = None,
//    status: Option[String] = None,
//    levelOfDetail: Option[LevelOfDetail.Value] = Some(LevelOfDetail.Partial),
//    dates: Option[String] = None,
//    languages: List[String] = Nil,
//    scripts: List[String] = Nil,
//    sources: Option[String] = None,
//    maintenanceNotes: Option[String] = None
//  ) extends Description {
//
//}
