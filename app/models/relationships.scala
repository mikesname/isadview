package models.relationships

import models._
import neo4j.data._

case object Describes extends Neo4jRelationship {
      val indexName = "describes"
}

case object LocatesInTime extends Neo4jRelationship {
      val indexName = "locatesInTime"
}

case object LocatesInSpace extends Neo4jRelationship {
  val indexName = "locatesInSpace"
}

case object HeldBy extends Neo4jRelationship {
  val indexName = "heldBy"
}

case object HasCollection extends Neo4jRelationship {
  val indexName = "hasCollection"
}

case object Contains extends Neo4jRelationship {
  val indexName = "contains"
}

case object AddressOf extends Neo4jRelationship {
  val indexName = "addressOf"
}

case object MentionedIn extends Neo4jRelationship {
  val indexName = "mentionedIn"
}

case object CreatedBy extends Neo4jRelationship {
  val indexName = "createdBy"
}




