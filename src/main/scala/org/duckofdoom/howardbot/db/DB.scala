package org.duckofdoom.howardbot.db

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import org.duckofdoom.howardbot.db.dto._

import scala.concurrent.ExecutionContext

trait DB {
  def getUser(userId:Integer) : Option[User]
//  def getFavourtes(userId:Integer) : Option[[String]]
}

class DoobieDB(driver:String, connectUrl:String, user:String, password:String) extends DB {
  
  implicit val executionContext = ExecutionContext.global
  implicit val contextShift = IO.contextShift(executionContext)
  
  private val transactor = Transactor.fromDriverManager[IO](driver, connectUrl, user, password, executionContext)

  override def getUser(userId: Integer): Option[User] = {
    throw new NotImplementedError()
  }
}
