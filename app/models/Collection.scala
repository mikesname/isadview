package models

import solr._
import neo4j.data._
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

object Collection extends Neo4jDataSource[Collection] {

  val indexName = "collection" 

  // JSON Constructor...
  def apply(data: net.liftweb.json.JsonAST.JValue): Collection = {
    new Collection(
      // adjust these as appropriate!
      id = idFromUrl((data \ "self").extractOpt[String]),
      slug = (data \ "data" \ "slug").extractOpt[String],
      createdOn = (data \ "data" \ "created_on").extractOpt[String].map(new DateTime(_)),
      updatedOn = (data \ "data" \ "updated_on").extractOpt[String].map(new DateTime(_)),
      description = CollectionDescription(
        identity = CollectionIdentity(
          identifier = (data \ "data" \ "identifier").extractOpt[String].getOrElse(""),
          name = (data \ "data" \ "name").extractOpt[String].getOrElse(""),
          levelOfDescription = (data \ "data" \ "level_of_description").extractOpt[Int],
          extentAndMedium = (data \ "data" \ "extent_and_medium").extractOpt[String]
        ),
        context = CollectionContext(
          archivalHistory = (data \ "data" \ "archival_history").extractOpt[String],
          acquisition = (data \ "data" \ "acquisition").extractOpt[String]
        ),
        content = CollectionContent(
          scopeAndContent = (data \ "data" \ "scope_and_content").extractOpt[String],
          appraisal = (data \ "data" \ "appraisal").extractOpt[String],
          accrurals = (data \ "data" \ "accrurals").extractOpt[String]
        ),
        conditions = CollectionConditions(
          conditionsOfAccess = (data \ "data" \ "conditions_of_access").extractOpt[String],
          conditionsOfReproduction = (data \ "data" \ "conditions_of_reproduction").extractOpt[String],
          languages = (data \ "data" \ "languages").extractOpt[String].getOrElse("").split(",").toList,
          scripts = (data \ "data" \ "scripts").extractOpt[String].getOrElse("").split(",").toList,
          physicalCharacteristics = (data \ "data" \ "physical_characteristics").extractOpt[String],
          findingAids = (data \ "data" \ "finding_aids").extractOpt[String]
        ),
        materials = CollectionMaterials(
          (data \ "data" \ "location_of_materials").extractOpt[String],
          (data \ "data" \ "location_of_copies").extractOpt[String],
          (data \ "data" \ "related_units_of_description").extractOpt[String]
        ),
        control = CollectionControl(
          (data \ "data" \ "rules").extractOpt[String],
          (data \ "data" \ "languages_of_description").extractOpt[String].getOrElse("").split(",").toList,
          (data \ "data" \ "scripts_of_description").extractOpt[String].getOrElse("").split(",").toList,
          (data \ "data" \ "sources").extractOpt[String]
        ),
        admin = CollectionAdmin(
          publicationStatus = (data \ "data" \ "publication_status").extractOpt[Int].getOrElse(0)
        )
      )
    )
  }

  override def fetchByFieldOption(field: String, value: String): Promise[Option[Collection]] = {
    val params = Map(
      "index_name" -> indexName,
      "key" -> field,
      "query_string" -> value,
      "inRels" -> List("describes", "locatesInTime", "locatesInSpace"),
      "outRels" -> List("heldBy", "createdBy")
    )
    gremlin("query_exact_index_with_related1", params).map(response => {
      val items = getJson(response)
      var collection = apply(items \ "item")
      collection = (items \ "describes").children.foldLeft(collection) { (c, json) =>
        c.withKeyword(Keyword(json))
      }
      collection = (items \\ "createdBy").children.foldLeft(collection) { (c, json) =>
        c.withCreator(Authority(json))
      }
      collection = (items \\ "locatesInTime").children.foldLeft(collection) { (c, json) =>
        c.withDate(FuzzyDate(json))
      }
      collection = (items \\ "locatesInSpace").children.foldLeft(collection) { (c, json) =>
        c.withPlace(Place(json))
      }
      collection = (items \\ "heldBy").children.foldLeft(collection) { (c, json) =>
        c.copy(repository=Some(Repository(json)))
      }
      collection = (items \\ "parents").children.foldLeft(collection) { (c, json) =>
        c.copy(parents = c.parents ++ List(Collection(json)))
      }
      collection = (items \\ "children").children.foldLeft(collection) { (c, json) =>
        c.copy(children = c.children ++ List(Collection(json)))
      }
      Some(collection)
    })
  }

  // Bare minimum factory constructor
  def apply(identifier: String, slug: String, name: String): Collection = new Collection(
    slug = Some(slug),
    createdOn = Some(new DateTime()),
    description = CollectionDescription(
      identity = CollectionIdentity(
        name = name,
        identifier = identifier
      ),
      context = CollectionContext(),
      content = CollectionContent(),
      conditions = CollectionConditions(),
      materials = CollectionMaterials(),
      control = CollectionControl(),
      admin = CollectionAdmin(
        publicationStatus = 0
      )
    )
  )
}

case class Collection(
  val id: Long = -1,
  val slug: Option[String] = None,
  val description: CollectionDescription,
  val createdOn: Option[DateTime] = None,
  val updatedOn: Option[DateTime] = None,
  val repository: Option[Repository] = None,
  val creators: List[Authority] = Nil,
  val places: List[Place] = Nil,
  val keywords: List[Keyword] = Nil,
  val parents: List[Collection] = Nil, // parent :: grandparent :: greatgrandparent :: _* :: Nil
  val children: List[Collection] = Nil
) extends Neo4jSlugModel with CrudUrls with SolrModel {
  def name = description.identity.name
  def summary = description.content.scopeAndContent
  val detailUrl = controllers.routes.Collections.detail(slug=slug.getOrElse(""))
  val editUrl = controllers.routes.Collections.edit(slug=slug.getOrElse(""))
  val deleteUrl = controllers.routes.Collections.confirmDelete(slug=slug.getOrElse(""))

  // FIXME: This is really ugly and reqires knowing way too much about the
  // details of persistance. Ideally we'd just pass a Map of relationship
  // to list of items, but the persister also needs to know the index name
  // for the subordinate items, which is can only get from the companion
  // object class
  override def getSubordinateItems = Map(
    "locatesInTime" -> description.identity.dates.filterNot(_.startDate.isEmpty).map { d =>
      Map(
        "index_name" -> FuzzyDate.indexName,
        "data" -> d.toMap)
    }
  )

  override def getIncomingSubordinateRelations = List("locatesInTime")

  def toMap = {
    Map(
      "slug" -> slug,
      Collection.TypeKey -> Collection.indexName,
      "created_on" -> createdOn.map(formatDate(_)),
      "updated_on" -> updatedOn.map(formatDate(_))
    ) ++ description.toMap
  }

  def toSolrDoc = {
    require(id > 0 && slug.isDefined, "Item %d has no slug; cannot create Solr index!".format(id))
    Map(
      "id" -> id,
      "slug" -> slug,
      "django_ct" -> ("portal." + Collection.indexName), // Legacy!!!
      "name" -> name,
      "tags_exact" -> keywords.map(_.text),
      "description" -> description.content.scopeAndContent,
      "repository" -> repository.map(_.name),
      "languages" -> description.conditions.languages.filterNot(_==""),
      "languages_of_description" -> description.control.languagesOfDescription.filterNot(_==""),
      "location_of_materials" -> repository.flatMap(_.countryCode).filterNot(_==""),
      "start_date" -> description.identity.dates.headOption.map(fd => fd.startDate.map(formatSolrDate(_))),
      "end_date" -> description.identity.dates.lastOption.map(d =>
          d.endDate.map(formatSolrDate(_)).getOrElse(d.startDate.map(formatSolrDate(_)))),
      "years" -> yearRange,
      "repository_slug" -> repository.map(_.slug),
      "tags" -> List(),
      "publication_status" -> description.admin.publicationStatus,
      "text" -> views.txt.search.collection(description).toString.replaceAll("\n{2,}", "\n\n")
    )
  }

  /*
   * Get an integer range of years spanned by this collection
   */
  def yearRange: List[Int] = {
    description.identity.dates.flatMap { fd =>
      (fd.startDate, fd.endDate) match {
        case (Some(s), Some(e)) => s.getYear.to(e.getYear)
        case (Some(s), None) => List(s.getYear)
        case (None, None) => Nil
      }
    }.sorted
  }

  def withSlug(newSlug: String) = copy(slug=Some(newSlug))
  def withCreator(creator: Authority) = copy(creators = creators ++ List(creator))
  def withDate(date: FuzzyDate) = copy(description=description.withDate(date))
  def withKeyword(kw: Keyword) = copy(keywords = keywords ++ List(kw))
  def withPlace(place: Place) = copy(places = places ++ List(place))
}

case class CollectionDescription(
  val identity: CollectionIdentity,
  val context: CollectionContext,
  val content: CollectionContent,
  val conditions: CollectionConditions,
  val materials: CollectionMaterials,
  val control: CollectionControl,
  val admin: CollectionAdmin
) {
  def withDate(date: FuzzyDate) = copy(identity=identity.withDate(date))
  def withDates(dates: List[FuzzyDate]) = dates.foldLeft(this)((c, d) => c.withDate(d))
  def toMap = {
    identity.toMap ++
    context.toMap ++
    content.toMap ++
    conditions.toMap ++
    materials.toMap ++
    control.toMap ++
    admin.toMap
  }
}


case class CollectionIdentity(
  val identifier: String = "",
  val name: String = "",
  val dates: List[FuzzyDateDescription] = Nil,
  val levelOfDescription: Option[Int] = Some(0),
  val extentAndMedium: Option[String] = None
) {
  def otherNames = Nil
  def toMap = Map(
    "identifier" -> identifier,
    "name" -> name,
    "level_of_description" -> levelOfDescription,
    "extent_and_medium" -> extentAndMedium
  )
  def withDate(date: FuzzyDate) = copy(dates=dates ++ List(date.description))
}

case class CollectionContext(
  val archivalHistory: Option[String] = None,
  val acquisition: Option[String] = None
) {
  def toMap = Map(
    "archival_history" -> archivalHistory,
    "acquisition" -> acquisition
  )
}

case class CollectionContent(
  val scopeAndContent: Option[String] = None,
  val appraisal: Option[String] = None,
  val accrurals: Option[String] = None,
  val systemOfArrangement: Option[String] = None
) {
  def toMap = Map(
    "scope_and_content" -> scopeAndContent,
    "appraisal"  -> appraisal,
    "accrurals" -> accrurals,
    "system_of_arrangement" -> systemOfArrangement
  )
}

case class CollectionConditions(
  val legalStatus: Option[String] = None,
  val conditionsOfAccess: Option[String] = None,
  val conditionsOfReproduction: Option[String] = None,
  val languages: List[String] = Nil,
  val scripts: List[String] = Nil,
  val physicalCharacteristics: Option[String] = None,
  val findingAids: Option[String] = None
) {
  def toMap = Map(
    "legal_status" -> legalStatus,
    "conditions_of_access" -> conditionsOfAccess,
    "conditions_of_reproduction" -> conditionsOfReproduction,
    "languages" -> languages.mkString(","),
    "scripts" -> scripts.mkString(","),
    "physical_characteristics" -> physicalCharacteristics,
    "finding_aids" -> findingAids
  )
}

case class CollectionMaterials(
  val locationOfOriginals: Option[String] = None,
  val locationOfCopies: Option[String] = None,
  val relatedUnitsOfDescription: Option[String] = None
) {
  def toMap = Map(
    "location_of_originals" -> locationOfOriginals,
    "location_of_copies" -> locationOfCopies,
    "related_units_of_description" -> relatedUnitsOfDescription
  )
}

case class CollectionNotes() {
  def toMap = Map()
}

case class CollectionControl(
  val rules: Option[String] = None,
  val languagesOfDescription: List[String] = Nil,
  val scriptsOfDescription: List[String] = Nil,
  val sources: Option[String] = None
) {
  def toMap = Map(
    "rules" -> rules,
    "languages_of_description" -> languagesOfDescription.mkString(","),
    "scripts_of_description" -> scriptsOfDescription.mkString(","),
    "sources" -> sources
  )
}

case class CollectionAdmin(
  val publicationStatus: Int = 0
) {
  def toMap = Map(
    "publication_status" -> publicationStatus
  )
}
