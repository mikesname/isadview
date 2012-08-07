package importers

import play.api.libs.concurrent.execution.defaultContext

object Cmd {

  def doImport(importer: Importer[xml.NodeSeq], format: String, file: String, repoId: String) = {
    XMLPullParser.processSource(format, io.Source.fromFile(file)) { doc =>
      importer.docToGeoff(repoId, doc).map { line =>
        println(line)
      }
    }
  }

  def repositoryId(repo: models.Repository) = "repo%d".format(repo.id)

  def main(args: Array[String]): Int = args match {
    case Array(repo, format, file) => {
      models.Repository.fetchByFieldOption("slug", repo).map { repooption =>
        repooption match {
          case Some(repository) => {
            val importer: Importer[xml.NodeSeq] = format match {
              case "ead" => EAD
              case "doc" => USHMM
              case x => sys.error("Unknown importer format: %s".format(x))
            }
            doImport(importer, format, file, repositoryId(repository))
          }
          case None => sys.error("Unable to find repository with slug: %s".format(repo))
        }
      }
      0
    }
    case _ => {
      println("Usage: <repository-slug> <format> <file>")
      1
    }
  }
}

