package controllers

import play.api._
import play.api.mvc._

import org.neo4j.graphdb._
import org.neo4j.rest.graphdb.RestGraphDatabase
import org.neo4j.scala._

import app.models
import app.forms.{Forms => DescForms}



object Description extends Controller with Neo4jWrapper with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
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
  private def getFromLookup(slug: String): Option[models.Description] = {
    withTx { implicit ds =>
      val node = ds.gds.index().forNodes(descIndex).query(lookupProp, slug).getSingle()
      node.toCC[models.Description]
    }           
  }

  def create = Action { implicit request => 
    Ok(views.html.descriptionForm(
        form=DescForms.descriptionForm,
        action=routes.Description.createPost)
    )
  }

  def update(slug: String) = Action { implicit request =>
    val desc = getFromLookup(slug)
    desc match {
      case Some(d) => Ok(views.html.descriptionForm(
            form=DescForms.descriptionForm.fill(d),
            action=routes.Description.updatePost(slug))
      )
      case _ => BadRequest("Unable to case description to case class (???)")
    }
  }
  
  def createPost = Action { implicit request =>
    DescForms.descriptionForm.bindFromRequest.fold(
      errors => BadRequest(views.html.descriptionForm(
            form=errors,
            action=routes.Description.createPost)
      ),
      desc => {
        withTx { implicit ds =>
          val node = createNode(desc)
          val nIndex = getNodeIndex(descIndex).get
          for ((key, value) <- getCCParams(desc)) {
            nIndex += (node, key, value.toString)
          }
          node
        }
        Redirect(routes.Description.detail(desc.slug))
      }
    )
  }

  def updatePost(slug: String) = Action { implicit request =>
    DescForms.descriptionForm.bindFromRequest.fold(
      errors => BadRequest(views.html.descriptionForm(
            form=errors, 
            action=routes.Description.updatePost(slug))
      ),
      desc => {
        val node = withTx { implicit ds =>
          val node = ds.gds.index().forNodes(descIndex).query(lookupProp, slug).getSingle()
          val nIndex = getNodeIndex(descIndex).get
          for ((key, value) <- getCCParams(desc)) {
            // update node values as well as indexes
            node(key) = value
            nIndex -= (node, key)
            nIndex += (node, key, value.toString)
          }
          node
        }
        Redirect(routes.Description.detail(desc.slug))
      }
    )
  }

  def detail(slug:String) = Action { implicit request =>
    val desc = getFromLookup(slug)
    desc match {
      case Some(d) => Ok(views.html.descriptionDetail(desc=d))
      case _ => BadRequest("Unable to case description to case class (???)")
    }
  }
}

