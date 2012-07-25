package test

import play.api.test.Helpers.running
import play.api.test.FakeApplication

import models.Repository

import scala.io.Source
import controllers.Collections.processSource
import importers.USHMM

object GeoffToNeo4j {
  val GEOFFMAXLINES = 200 // number of Geoff statements to import at a time

  def importGeoff(repo: Repository, geofffile: String) = {
    println("Importing geoff for: " + repo)
    //Repository.importGeoff(repo, Source.fromFile(geofffile).getLines.toList).map { params =>
    //  println("Return params: " + params)
    //}
  }

  def main(args: Array[String]) = {
    args match {
      case Array(reposlug, gfile, _*) => {
        running(FakeApplication()) {
          println("Fetching repository with slug: " + reposlug)
          Repository.fetchByFieldOption("slug", reposlug).map { maybeRepo => 
            println("Got repo: " + maybeRepo)
            maybeRepo match {
              case Some(repo) => {
                println("Got repo: " + repo)
                //importGeoff(repo, gfile)
              }
              case None => sys.error("Repository not found: " + reposlug)
            }
          }
        }
      }
      case _ => sys.error("Usage: <repo-slug> <xmlfile>")
    }
  }
}


