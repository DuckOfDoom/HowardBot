package org.duckofdoom.howardbot.server

import cats.syntax.option._
import org.duckofdoom.howardbot.bot.services.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.services.{ResponseFormat, ResponseService, StatusService}
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.utils.FileUtils
import org.duckofdoom.howardbot.bot.data.ItemsProvider
import scalatags.Text.all._

trait ServerResponseService {
  def home(): String
  def menu(): String
  def menuJson(): String
  def getUsers(): String
  def getUser(userId: Int): String
  def putRandomUser(): String
  def show(itemId: Int): String
  def parse(): String
}

class ServerResponseServiceImpl(
    implicit statusInfoProvider: StatusService,
    itemDataProvider: ItemsProvider,
    responseService: ResponseService,
    db: DB
) extends ServerResponseService {

  implicit val responseFormat: ResponseFormat = ResponseFormat.TextMessage

  override def home(): String = {
    statusInfoProvider.getStatusInfoHtml
  }

  override def menu(): String = {
    val beers = itemDataProvider.beers

    frag(
      s"Всего пивасов: ${beers.length}",
      br(),
      beers.map { beer =>
        frag(
          a(href := beer.link.getOrElse("?"))("🍺 " + beer.name.getOrElse("name = ?")),
          beer.rating.map { case (v1, _) => s" $v1" }.getOrElse(" rating = ?").toString,
          br(),
          s"Стиль: ${beer.style
            .map(style => {
              style.toString
            })
            .getOrElse("style = ? ")}",
          br(),
          s"Пивоварня: ${beer.breweryInfo.name.getOrElse("breweryInfo.name = ?")}",
          br(),
          beer.draftType match {
            case Some(dr) =>
              dr + " - " + beer.price
                .map { case (c, price) => c + price }
                .getOrElse("price = ?")
            case None => "On Deck"
          },
          br(),
          s"${beer.description.getOrElse("description = ?")}",
          br(),
          br()
        )
      }
    ).render
  }

  override def menuJson(): String = {
    s"<pre>${FileUtils
      .readFile(ItemsProvider.savedMenuFilePath)
      .getOrElse(s"Can't read file '${ItemsProvider.savedMenuFilePath}'")}</pre>"
  }

  override def show(itemId: Int): String = {
    responseService.mkBeerResponse(itemId)._1
  }

  override def parse(): String = {

    val sb = new StringBuilder()
    itemDataProvider.beers
      .sortBy(_.id)
      .foreach(i => {
        sb.append(
          i.toString
            .replace("\n", "<br>")
            .replace("\t", "<nbsp>")
        )
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
