package models.sql

import play.api._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import java.util.Date

// -- Users

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission


case class User(id: Long, name: String, email: String) {
  
  lazy val associations: Seq[Association] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_association 
        join openid_user on openid_association.user_id = openid_user.id 
        where openid_user.id = {id} 
      """
    ).on('id -> id).as(Association.withUser *)
  }

  def addAssociation(assoc: String): User = DB.withConnection { implicit connection => 
    SQL(
      """
        INSERT INTO openid_association (user_id, openid_url) VALUES ({user_id},{url})
        ON DUPLICATE KEY UPDATE openid_url = {url}
      """
    ).on('user_id -> id, 'url -> assoc)
    this
  }
}

object User {

  val simple = {
     get[Long]("openid_user.id") ~
     get[String]("openid_user.email") ~
     get[String]("openid_user.name") map {
       case id ~ name ~ email => User(id, name, email)
     }
  }

  def findAll: Seq[User] = DB.withConnection { implicit connection =>
    SQL(
      """select * from openid_user"""
    ).as(User.simple *)
  }

  def authenticate(id: Long, email: String, name: String): User = DB.withConnection { implicit connection =>
    SQL(
      """
        INSERT INTO openid_user (id,name,email) VALUES ({id},{name},{email})
        ON DUPLICATE KEY UPDATE id = {id}
      """
    ).on('id -> id, 'email -> email, 'name -> name).executeUpdate()
    User(id, name, email)
  }

  def authenticate(url: String): Option[User] = DB.withConnection { implicit connection =>
    SQL(
      """
        SELECT * FROM openid_user 
          JOIN openid_association ON openid_association.user_id = openid_user.id
          WHERE openid_association.openid_url = {url}
      """
    ).on('url -> url).as(User.simple.singleOpt)
  }

  def findById(id: Long): Option[User] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_user where id = {id}
      """
    ).on('id -> id).as(User.simple.singleOpt)
  }

  def checkUniqueUsername(username: String): Boolean = DB.withConnection { implicit connection =>
    SQL(
      """SELECT COUNT(name) FROM openid_user WHERE name = {name}"""
    ).on('name -> username).as(scalar[Long].single) == 0L
  }

  def create(name: String, email: String): Option[User] = DB.withConnection { implicit connection =>
    SQL(
      """INSERT INTO openid_user (id, name, email) VALUES (DEFAULT, {name},{email})"""
    ).on('name -> name, 'email -> email).executeUpdate
    SQL(
      """SELECT * FROM openid_user WHERE id = currval('openid_user_id_seq')"""
    ).as(User.simple.singleOpt)
  }
}

// -- Associations

case class Association(id: Long, userid: Long, url: String, user: Option[User] = None) {
   
  lazy val users: Seq[User] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_user 
        join openid_association on openid_association.user_id = openid_user.id
        where openid_association.id = {id}
      """
    ).on('id -> id).as(User.simple *)
  }
}

object Association {

  val simple = {
    get[Long]("openid_association.id") ~
    get[Long]("openid_association.user_id") ~
    get[String]("openid_association.openid_url") map {
      case id~userid~url => Association(id, userid, url, None)
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
        select * from openid_association join openid_user on openid_association.user_id =  openid_user.id
      """
    ).as(Association.withUser *)
  }

  def findByUrl(url: String): Option[Association] = DB.withConnection { implicit connection =>
    SQL(
      """
        select * from openid_association
          join openid_user on openid_association.user_id =  openid_user.id where
        openid_association.openid_url = {url}
      """
    ).on('url -> url).as(Association.withUser.singleOpt)
  }
}

