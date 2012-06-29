package models.sql

import play.api._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import java.util.Date

// -- Users

case class User(id: Long, name: String, email: String) {
  
  lazy val associations: Seq[Association] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_associations 
        join users on openid_associations.user_id = users.id 
        join subscribers on subscribers.event_id = events.id
        where users.id = {id} 
      """
    ).on('id -> id).as(Association.withUser *)
  }
}

object User {

  val simple = {
     get[Long]("users.id") ~
     get[String]("users.email") ~
     get[String]("users.name") map {
       case id ~ name ~ email => User(id, name, email)
     }
  }

  def authenticate(id: Long, email: String, name: String): User = DB.withConnection { implicit connection =>
    SQL(
      """
        INSERT INTO users (id,name,email) VALUES ({id},{name},{email})
        ON DUPLICATE KEY UPDATE id = {id}
      """
    ).on('id -> id, 'email -> email, 'name -> name).executeUpdate()
    User(id, name, email)
  }

  def findByEmail(email: String): Option[User] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from users where email = {email}
      """
    ).on(
      'email -> email
    ).as(User.simple.singleOpt)
  }
}

// -- Associations

case class Association(id: Long, url: String, handle: String, secret: String,
    issued: Long, lifetime: Long, assoc: String, user: Option[User] = None) {
   
  lazy val users: Seq[User] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from users 
        join openid_associations on openid_associations.user_id = users.id
        where openid_associations.id = {id}
      """
    ).on('id -> id).as(User.simple *)
  }
}

object Association {

  val simple = {
    get[Long]("openid_associations.id") ~
    get[String]("openid_associations.server_url") ~
    get[String]("openid_associations.handle") ~
    get[String]("openid_associations.secret") ~
    get[Long]("openid_associations.issued") ~
    get[Long]("openid_associations.lifetime") ~
    get[String]("openid_associations.assoc_type") map {
      case id~url~handle~secret~issued~lifetime~assoc => Association(
            id, url, handle, secret, issued, lifetime, assoc, None)
    }
  }

  val withUser = {
    simple ~ User.simple map {
      case association~user => association.copy(user = Some(user))
    }
  }

  def findAll: Seq[Association] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_associations join users on openid_associations.user_id =  users.id
      """
    ).as(Association.withUser *)
  }

  def findById(id: Long): Option[Association] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_associations join users on openid_associations.user_id =  users.id where
        openid_associations.id = {id}
      """
    ).on('id -> id).as(Association.withUser.singleOpt)
  }
}

