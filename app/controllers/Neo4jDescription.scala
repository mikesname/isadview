package controllers

import play.api._
import play.api.mvc._

import org.neo4j.graphdb._
import org.neo4j.rest.graphdb.RestGraphDatabase
import org.neo4j.scala._

import app.models
import app.forms.{Forms => DescForms}



object Neo4jDescription extends Controller with Neo4jWrapper with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
  var descIndex = "descriptions"
  var lookupProp = "slug"
  override def NodeIndexConfig = (descIndex, Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: Nil
  override def uri = new java.net.URI(play.Play.application.configuration.getString("neo4j.path"))
  override def userPw: Option[(String, String)] = None

  // deconstruct a case class to a Map
  private def getCCParams(cc: AnyRef) =
    (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(cc))
  }

  // fetch
  private def getFromLookup(slug: String): Option[models.Neo4jDescription] = {
    withTx { implicit ds =>
      val node = ds.gds.index().forNodes(descIndex).query(lookupProp, slug).getSingle()
      node.toCC[models.Neo4jDescription]
    }           
  }

  def create = Action { implicit request => 
    Ok(views.html.descriptionForm(
        form=DescForms.descriptionForm,
        action=routes.Neo4jDescription.createPost)
    )
  }

  def update(slug: String) = Action { implicit request =>
    val desc = models.Neo4jDescription.get(slug)
    desc match {
      case Some(d) => Ok(views.html.descriptionForm(
            form=DescForms.descriptionForm.fill(d),
            action=routes.Neo4jDescription.updatePost(slug))
      )
      case _ => BadRequest("Unable to case description to case class (???)")
    }
  }
  
  def createPost = Action { implicit request =>
    DescForms.descriptionForm.bindFromRequest.fold(
      errors => BadRequest(views.html.descriptionForm(
            form=errors,
            action=routes.Neo4jDescription.createPost)
      ),
      desc => {
        desc.save
        Redirect(routes.Neo4jDescription.detail(desc.slug))
      }
    )
  }

  def updatePost(slug: String) = Action { implicit request =>
    DescForms.descriptionForm.bindFromRequest.fold(
      errors => BadRequest(views.html.descriptionForm(
            form=errors, 
            action=routes.Neo4jDescription.updatePost(slug))
      ),
      desc => {
        desc.save
        Redirect(routes.Neo4jDescription.detail(desc.slug))
      }
    )
  }

  def detail(slug:String) = Action { implicit request =>
    val desc = models.Neo4jDescription.get(slug)
    println("Got: %s".format(desc))
    desc match {
      case Some(d) => Ok(views.html.descriptionDetail(desc=d))
      case _ => BadRequest("Unable to case description to case class (???)")
    }
  }
}

