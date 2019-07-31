package org.duckofdoom.howardbot.db

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect.{ContextShift, IO}
import cats.implicits._
import org.duckofdoom.howardbot._
import org.duckofdoom.howardbot.db.dto._
import slogging.StrictLogging

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

trait DB {
  def users: List[User]
  def getUser(userId: Long): Option[User]
  def putUser(userId: Int, username: String, firstName:String, lastName: String): Option[User]
}

class DoobieDB(config: PostgresConfig) extends DB 
  with StrictLogging { 

  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val contextShift: ContextShift[IO] = IO.contextShift(executionContext)

  private val transactor = Transactor.fromDriverManager[IO](
    config.driver,
    config.connectUrl,
    config.user,
    config.password,
    executionContext
  )

  createUsersTable()
  
  private def createUsersTable(): Unit = {
    sql"""
    CREATE TABLE IF NOT EXISTS users (  
      id   SERIAL,
      userId INTEGER,
      username VARCHAR,
      firstName VARCHAR,
      lastName VARCHAR
    )"""
      .update
      .run
      .transact(transactor)
      .unsafeRunSync
  }

  override def users: List[User] = {
    val users = sql"select * from users"
      .query[User]
      .to[List]
      .transact(transactor)
      .unsafeRunSync()
    
    logger.info(s"Selected ${users.length} users")
    users
  }

  override def getUser(id: Long): Option[User] = {
    selectUserQuery(id)
      .transact(transactor)
      .unsafeRunSync()
  }

  override def putUser(userId: Int, username: String, firstName:String, lastName: String): Option[User] = {
    logger.info(s"Creating new user: $userId / $username / $firstName / $lastName")
    
    val conn = for {
      _  <- sql"""
            insert into users (userId, username, firstName, lastName) 
            values ($userId, $username, $firstName, $lastName)""".update.run

      id <- sql"select lastval()".query[Long].unique
      p  <- selectUserQuery(id)
    } yield p
    
    conn.transact(transactor).unsafeRunSync()
  }
  
  private def selectUserQuery(id:Long) : ConnectionIO[Option[User]] = {
    sql"select * from users where id=$id"
      .query[User]
      .option
  }
}
