import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "isadview"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "com.github.seratch" %% "scalikesolr" % "3.6.0",
      "net.databinder" %% "dispatch-http" % "0.8.8" withSources,
      "org.scala-tools.time" % "time_2.9.1" % "0.5",
      "net.liftweb" % "lift-json-ext_2.9.1" % "2.4-M5",
      "org.codehaus.groovy" % "groovy-all" % "1.8.6",
      "postgresql" % "postgresql" % "9.1-901.jdbc4",
      "org.ocpsoft.prettytime" % "prettytime" % "1.0.8.Final",
      "jp.t2v" % "play20.auth_2.9.1" % "0.3-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
      resolvers += "t2v.jp repo" at "http://www.t2v.jp/maven-repo/"
    )

}
