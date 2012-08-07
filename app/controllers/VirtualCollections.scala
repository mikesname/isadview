package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import net.liftweb.json

import com.codahale.jerkson.Json._

import models._
import forms.UserForm


object VirtualCollections extends AuthController with ControllerHelpers {
  def detail(id: Long) = authorizedUserProfileAction(models.sql.NormalUser) { implicit user => implicit request =>
    user.profile.flatMap(_.virtualCollections.find(_.id==id)).map { vc =>
      Ok(views.html.virtualcollection.detail(user, vc))
    }.getOrElse(authorizationFailed(request))
  }

  def update(id: Long) = authorizedUserProfileAction(models.sql.NormalUser) { implicit user => implicit request =>
    user.profile.flatMap(_.virtualCollections.find(_.id==id)).map { vc =>
      val form = UserForm.virtualCollection.fill(vc.description)
      val action = routes.VirtualCollections.updatePost(id)
      Ok(views.html.user.vcform(user, form, action))
    }.getOrElse(authorizationFailed(request))
  }

  def updatePost(id: Long) = authorizedUserProfileAction(models.sql.NormalUser) { implicit user => implicit request =>
    assert(user.profile.map(_.virtualCollections.map(_.id).contains(id)).getOrElse(false))
    user.profile.flatMap(_.virtualCollections.find(_.id==id)).map { vc =>
      val action = routes.VirtualCollections.updatePost(id)
      UserForm.virtualCollection.bindFromRequest.fold(
        errorForm => BadRequest(views.html.user.vcform(user, errorForm, action)),
        vcdesc => {
          Async {
            VirtualCollection.persist(vc.id, vc.copy(description=vcdesc)).map { updated =>
              Redirect(routes.VirtualCollections.detail(vc.id))
            }
          }
        }
      )
    }.getOrElse(authorizationFailed(request))
  }

  def delete(id: Long) = authorizedUserProfileAction(models.sql.NormalUser) { implicit user => implicit request =>
    user.profile.flatMap(_.virtualCollections.find(_.id==id)).map { vc =>
      val action = routes.VirtualCollections.deletePost(id)
      Ok(views.html.basedelete(c=vc, action=action)(Some(user), request))
    }.getOrElse(authorizationFailed(request))
  }

  def deletePost(id: Long) = authorizedUserProfileAction(models.sql.NormalUser) { implicit user => implicit request =>
    assert(user.profile.map(_.virtualCollections.map(_.id).contains(id)).getOrElse(false))
    Async {
      VirtualCollection.delete(id).map { res =>
        Redirect(routes.Users.profile)
      }
    }
  }

  def create = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    Ok(views.html.user.vcform(
        user, UserForm.virtualCollection, routes.VirtualCollections.createPost))
  }

  def createPost = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request => 
    UserForm.virtualCollection.bindFromRequest.fold(
      errorForm => BadRequest(views.html.user.vcform(
          user, errorForm, routes.VirtualCollections.createPost)),
      newvc => {
        // if we don't already have a profile, create one
        user.profile match {
          case None => throw sys.error("No user profile found!")
          case Some(profile) => createNew(profile, newvc)
        }
      }
    )
  }

  private def createNew(profile: UserProfile, vcdesc: VirtualCollectionDescription) = {
    Async {
      UserProfile.createVirtualCollection(profile, vcdesc).map { created =>
        Redirect(routes.Users.profile)
      }
    }
  }

  def saveItem(item: Long, vc: Long) = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    // TODO: Check virtual collection `vc` belongs to user!
    val action = routes.VirtualCollections.saveItemPost(item, vc)
    Ok(views.html.virtualcollection.saveItem(user, item, vc, action))
  }

  def saveItemPost(item: Long, vc: Long) = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    // TODO: Check virtual collection `vc` belongs to user!
    Async {
      VirtualCollection.createRelationship(vc, item, "contains").map { edge =>
        if (isAjaxRequest(request))
          Ok(generate(Map("ok" -> true)))
        else
          // TODO: Find out where the user came from!
          Redirect(routes.Search.home)
      }
    }
  }

  def removeItem(vc: Long, rel: Long) = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    assert(user.profile.map(_.virtualCollections.flatMap(_.items.map(_.edge.id)).contains(rel)).getOrElse(false))
    val action = routes.VirtualCollections.removeItemPost(vc, rel)
    Ok(views.html.virtualcollection.removeItem(user, rel, action=action))
  }

  def removeItemPost(vc: Long, rel: Long) = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    assert(user.profile.map(_.virtualCollections.flatMap(_.items.map(_.edge.id)).contains(rel)).getOrElse(false))
    Async {
      VirtualCollection.deleteRelationship(rel).map { edge =>
        if (isAjaxRequest(request))
          Ok(generate(Map("ok" -> true)))
        else
          // TODO: Find out where the user came from!
          Redirect(routes.VirtualCollections.detail(vc))
      }
    }
  }

  def saveItemToNew(item: Long) = authorizedUserProfileAction(models.sql.NormalUser) { user => implicit request =>
    // TODO: Check virtual collection `vc` belongs to user!
    UserForm.virtualCollection.bindFromRequest.fold(
      errorForm => BadRequest(views.html.user.vcform(
          user, errorForm, routes.VirtualCollections.create)),
      newvc => {
        // if we don't already have a profile, create one
        user.profile match {
          case None => throw sys.error("No user profile found!")
          case Some(profile) => {
            Async {
              UserProfile.createVirtualCollection(profile, newvc).flatMap { created =>
                println("Created virtual collection with id: " + created.id)
                println("Saving item to collection: %d -> %d".format(created.id, item))
                VirtualCollection.createRelationship(created.id, item, "contains").map { edge =>
                  if (isAjaxRequest(request))
                    Ok(generate(Map("ok" -> true, "name" -> created.name, "id" -> created.id)))
                  else
                    Redirect(routes.VirtualCollections.detail(created.id))
                }
              }
            }
          }
        }
      }
    )
  }
}
