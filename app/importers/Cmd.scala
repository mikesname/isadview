package importers

import scalax.io.{Resource,Codec => IOCodec}
import play.api.libs.concurrent.execution.defaultContext

object XmlToGeoff {
  def repositoryId(repo: models.Repository) = "repo%d".format(repo.id)

  def getGeoff(importer: Importer[xml.NodeSeq], format: String, infile: String, outfile: String, repoId: String) = {
    val output = Resource.fromFile(outfile)
    output.truncate(0)
    XMLPullParser.processSource(format, io.Source.fromFile(infile)) { doc =>
      output.writeStrings(importer.docToGeoff(repoId, doc), separator="\n")(IOCodec.UTF8)
    }
  }

  def main(args: Array[String]): Int = args match {
    case Array(repo, format, infile, outfile) => {
      models.Repository.fetchByFieldOption("slug", repo).map { repooption =>
        repooption match {
          case Some(repository) => {
            val importer: Importer[xml.NodeSeq] = format match {
              case "ead" => EAD
              case "doc" => USHMM
              case x => sys.error("Unknown importer format: %s".format(x))
            }
            getGeoff(importer, format, infile, outfile, repositoryId(repository))
          }
          case None => sys.error("Unable to find repository with slug: %s".format(repo))
        }
      }
      0
    }
    case _ => {  
      println("Usage: <repository-slug> <format> <infile> <outfile>")
      1
    }
  }
}

