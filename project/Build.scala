import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "isadview"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.neo4j" % "neo4j-kernel" % "1.8.M02" % "test" classifier "tests",
      "org.neo4j" % "neo4j" % "1.8.M02",
      "org.neo4j" % "neo4j-rest-graphdb" % "1.8-SNAPSHOT",
      "org.neo4j" % "neo4j-shell" % "1.8.M02",
      "com.tinkerpop.blueprints" % "blueprints-neo4j-graph" % "2.0.0",
      "com.github.seratch" %% "scalikesolr" % "(3.6,)",
      "org.neo4j" % "neo4j-scala" % "0.2.0-M1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
    )

}
