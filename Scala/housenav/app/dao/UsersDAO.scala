package dao

import scala.concurrent.Future
import javax.inject.Inject
import javax.inject.Singleton
import models.User
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.driver.H2Driver
import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait UserComponent { self: HasDatabaseConfigProvider[H2Driver] =>
  import driver.api._
  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def email = column[String]("email")
    def password = column[String]("password")
    def name = column[Option[String]]("name")
    def * = (id.?, email, password, name) <> (User.tupled, User.unapply _)
  }
}

@Singleton
class UsersDAO @Inject() (protected val dbConfigProvider: DatabaseConfigProvider) extends UserComponent
    with HasDatabaseConfigProvider[H2Driver] {
  import driver.api._

  private val Users = TableQuery[Users]
  db.run((Users.schema).create)

  def insert(user: User): Future[Int] = {
    db.run(Users += user)
  }

  def insert(users: Seq[User]): Future[Unit] =
    db.run(this.Users ++= users).map(_ => ())

  def update(id: Long, user: User): Future[Int] = {
    val userToUpdate = user.copy(Some(id))
    db.run(Users.filter(_.id === id).update(userToUpdate))
  }

  def getAll =
    db.run(Users.result)

  def findById(id: Long): Future[Option[User]] =
    db.run(Users.filter(_.id === id).result.headOption)

  def findByEmail(email: String): Future[Option[User]] = {
    db.run(Users.filter(_.email === email).result.headOption)
  }

  def authenticate(email: String, password: String): Future[Boolean] =
    db.run(Users.filter(u => u.email === email && u.password === password).result.headOption).map(!_.isEmpty)

  def deleteAll =
    db.run(Users.delete)

  def delete(id: Long) = {
    db.run(Users.filter(_.id === id).delete)
  }

}