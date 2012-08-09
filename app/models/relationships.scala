package models.relationships

import models._

case object Describes extends neo4j.Relationship {
      val indexName = "describes"
}

case object LocatesInTime extends neo4j.Relationship {
      val indexName = "locatesInTime"
}

case object LocatesInSpace extends neo4j.Relationship {
  val indexName = "locatesInSpace"
}

case object HeldBy extends neo4j.Relationship {
  val indexName = "heldBy"
}

case object HasCollection extends neo4j.Relationship {
  val indexName = "hasCollection"
}

case object Contains extends neo4j.Relationship {
  val indexName = "contains"
}

case object AddressOf extends neo4j.Relationship {
  val indexName = "addressOf"
}

case object MentionedIn extends neo4j.Relationship {
  val indexName = "mentionedIn"
}

case object CreatedBy extends neo4j.Relationship {
  val indexName = "createdBy"
}




