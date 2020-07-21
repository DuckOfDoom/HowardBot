package org.duckofdoom.howardbot.db

import cats.effect.{ContextShift, IO}
import doobie._
import doobie.implicits._
import doobie.util.log.{ExecFailure, ProcessingFailure}
import io.circe.Encoder
import io.circe.syntax._
import org.duckofdoom.howardbot._
import org.duckofdoom.howardbot.db.dto._
import slogging.StrictLogging

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

trait DB {
  def users: Seq[User]
  def getUser(userId: Long): Option[User]
  def getUserByTelegramId(userId: Long): Option[User]
  def putUser(userId: Long, firstName: String, lastName: Option[String], username: Option[String]): User
  def updateUser(user: User): User
}

class DoobieDB(config: PostgresConfig) extends DB with StrictLogging {
  
  import DBDataUtils._

  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val contextShift: ContextShift[IO]             = IO.contextShift(executionContext)

  implicit val logHandler: LogHandler = LogHandler {
    case ExecFailure(str, args, _, ex) =>
      logger.error(s"ExecFailure: $str\nArgs: $args\nException: $ex")
    case ProcessingFailure(str, args, _, _, ex) =>
      logger.error(s"ProcessingFailure: $str \nArgs: $args\n Exception: $ex")
//    case Success(str, args, t1, t2) => logger.info(s"$str \nArgs: $args, $t1, $t2")
    case _ => Unit
  }

  private val transactor = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    s"jdbc:postgresql:${config.database}",
    config.user,
    config.password,
    executionContext
  )

  initTable()

  private def initTable(): Unit = {
    sql"""
    CREATE TABLE IF NOT EXISTS users (  
      id SERIAL UNIQUE,
      userid INTEGER NOT NULL,
      firstname VARCHAR NOT NULL,
      lastname VARCHAR,
      username VARCHAR,
      state VARCHAR DEFAULT '{}' 
    )""".update.run
      .transact(transactor)
      .unsafeRunSync

    sql"""ALTER TABLE users ADD COLUMN IF NOT EXISTS cart VARCHAR DEFAULT '{}'""".update.run
      .transact(transactor)
      .unsafeRunSync
  }

  override def users: Seq[User] = {
    val users = sql"select * from users"
      .query[User]
      .to[List]
      .transact(transactor)
      .unsafeRunSync()

    logger.info(s"Selected ${users.length} users")
    users
  }

  override def getUserByTelegramId(userId: Long): Option[User] = {
    sql"select * from users where userId=$userId"
      .query[User]
      .option
      .transact(transactor)
      .unsafeRunSync()
  }

  override def getUser(id: Long): Option[User] = {
    selectUserQuery(id)
      .transact(transactor)
      .unsafeRunSync()
  }

  def putUser(userId: Long, firstName: String, lastName: Option[String], username: Option[String]): User = {
    logger.info(s"Creating new user: $userId / $firstName / $lastName / $username")

    val conn = for {
      _ <- sql"""
            insert into users (userid, firstname, lastname, username) 
            values ($userId, $firstName, $lastName, $username)""".update.run

      id <- sql"select lastval()".query[Long].unique
      p  <- sql"select * from users where id=$id".query[User].unique
    } yield p

    conn.transact(transactor).unsafeRunSync()
  }

  def updateUser(user: User): User = {
    logger.info(s"Updating user: $user")

    implicit val encoder: Encoder[UserState] = UserState.encoder

    sql"""UPDATE users  
    SET 
      userid = ${user.userId},
      firstname = ${user.firstName},
      lastname = ${user.lastName},
      username = ${user.username},
      state = ${user.state.asJson.toString.replaceAll("[\n\r\\s]", "")}      
    WHERE id = ${user.id}""".update.run
      .transact(transactor)
      .unsafeRunSync()
    
    user
  }

  private def selectUserQuery(id: Long): ConnectionIO[Option[User]] = {
    sql"select * from users where id=$id"
      .query[User]
      .option
  }
}
