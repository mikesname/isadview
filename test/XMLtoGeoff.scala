package test

import play.api.test.Helpers.running
import play.api.test.FakeApplication

import models.Repository

import scala.io.Source
import controllers.Collections.processSource
import importers.USHMM


object XMLtoGeoff {

  def getGeoff(repo: Repository, xmlfile: String) = {
    processSource(Source.fromFile(xmlfile)) { doc =>
      USHMM.docToGeoff("repo%d".format(repo.id), doc).map { geoffline =>
        println(geoffline)
      }
    }
  }

  def main(args: Array[String]) = {
    args match {
      case Array(reposlug, xmlfile, _*) => {
        running(FakeApplication()) {
          Repository.fetchByFieldOption("slug", reposlug).map { maybeRepo =>
            println("Running: " + maybeRepo)
            maybeRepo match {
              case Some(repo) => getGeoff(repo, xmlfile)
              case None => sys.error("Repository not found: " + reposlug)
            }
          }
        }
      }
      case _ => sys.error("Usage: <repo-slug> <xmlfile>")
    }
  }
}

