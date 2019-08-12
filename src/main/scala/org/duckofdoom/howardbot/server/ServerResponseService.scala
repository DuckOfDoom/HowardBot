package org.duckofdoom.howardbot.server

import org.duckofdoom.howardbot.bot.data.ItemDataProvider
import org.duckofdoom.howardbot.bot.{StatusProvider, ResponseService}
import org.duckofdoom.howardbot.db.DB
import cats.syntax.option._

trait ServerResponseService {
  def home(): String
  def menu(): String
  def getUsers(): String
  def getUser(userId: Int): String
  def putRandomUser(): String
  def show(itemId: Int): String
  def parse(): String
}

class ServerResponseServiceImpl(implicit statusInfoProvider: StatusProvider,
                                itemDataProvider: ItemDataProvider,
                                responseService: ResponseService,
                                db: DB)
    extends ServerResponseService {

  override def home(): String = {
    statusInfoProvider.getStatusInfoHtml
  }

  override def menu(): String = {
    responseService.mkMenuResponse("/")
  }

  override def show(itemId: Int): String = {
    responseService.mkItemResponse(itemId)._1
  }

  override def parse(): String = {
    
    val sb = new StringBuilder()
    itemDataProvider.allItems.toList
      .sortBy(_.id)
      .foreach(i => {
        sb.append(
          i.toString
            .replace("\n", "<br>")
            .replace("\t", "<nbsp>"))
        sb.append("<br><br>")
      })

    sb.toString
  }

  override def putRandomUser(): String = {
    db.putUser(
        scala.util.Random.nextInt(Int.MaxValue),
        faker.Name.first_name,
        faker.Name.first_name.some,
        faker.Name.last_name.some
      )
      .toString
  }

  override def getUsers(): String = {
    "Users:<br>" +
      db.users.foldLeft("")((s, u) => {
        s + (u.toString + "<br>")
      })
  }

  override def getUser(userId: Int): String = {
    db.getUser(userId).toString
  }

}
