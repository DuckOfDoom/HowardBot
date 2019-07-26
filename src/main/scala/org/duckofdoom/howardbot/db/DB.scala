package org.duckofdoom.howardbot.db

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import org.duckofdoom.howardbot._
import org.duckofdoom.howardbot.db.dto._

import scala.concurrent.ExecutionContext

trait DB {
  def createUser(user:User) : Option[Unit]
  def getUser(userId:Integer) : Option[User]
//  def getFavourtes(userId:Integer) : Option[[String]]
}

class DoobieDB(config: PostgresConfig ) extends DB {
  
  implicit val executionContext = ExecutionContext.global
  implicit val contextShift = IO.contextShift(executionContext)

  private val transactor = Transactor.fromDriverManager[IO](
    config.driver,
    config.connectUrl,
    config.user,
    config.password,
    executionContext
  )

  override def getUser(userId: Integer): Option[User] = {
    val users = sql"select * from users where id=$userId"
      .query[User]
      .stream
      .quick
      .unsafeRunSync
  }

  override def createUser(user: User): Option[Unit] = ???
}
