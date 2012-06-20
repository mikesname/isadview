package spring.models

import org.springframework.data.neo4j.annotation._

@NodeEntity
class Collection(
  @Indexed val name: String,
  @Indexed val slug: String,
  @Indexed val identifier: String
) {
    @GraphId
    var id: java.lang.Long = _
}
