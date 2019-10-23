package org.duckofdoom.howardbot.server

import cats.syntax.option._
import org.duckofdoom.howardbot.bot.services.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.services.{ResponseFormat, ResponseService, StatusService}
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.utils.FileUtils
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import scalatags.Text.all._

trait ServerResponseService {
  def home(): String
  def menuAvailable(): String
  def menuOnDeck(): String
  def menuOutOfStock(): String
  def menuFull(): String
  def menuChangelog(): String
  def menuRaw(): String
  def users(): String
  def getUser(userId: Int): String
  def putRandomUser(): String
  def show(itemId: Int): String
}

class ServerResponseServiceImpl(
    implicit statusInfoProvider: StatusService,
    itemDataProvider: ItemsProvider,
    responseService: ResponseService,
    db: DB
) extends ServerResponseService {

  implicit val responseFormat: ResponseFormat = ResponseFormat.TextMessage

  override def home(): String = {
    s"${statusInfoProvider.getStatusInfoHtml}\n" +
      frag(
        p(a(href := "/users")("Users")),
        p(a(href := "/menu/available")("Menu [Available]")),
        p(a(href := "/menu/ondeck")("Menu [On Deck]")),
        p(a(href := "/menu/outofstock")("Menu [Out Of Stock]")),
        p(a(href := "/menu/full")("Menu [Full]")),
        p(a(href := "/menu/raw")("Menu [Raw]")),
        p(a(href := "/menu/changelog")("Menu [Changelog]"))
      ).render
  }

  override def menuAvailable(): String = {
    mkMenuResponse(itemDataProvider.availableBeers)
  }
  
  override def menuOnDeck(): String = {
    mkMenuResponse(itemDataProvider.beers.filter(_.isOnDeck))
  }

  override def menuOutOfStock(): String = {
    mkMenuResponse(itemDataProvider.beers.filter(!_.isInStock))
  }

  override def menuFull(): String = {
    mkMenuResponse(itemDataProvider.beers)
  }

  private def mkMenuResponse(beers: Seq[Beer]): String = {
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

  override def menuChangelog(): String = {
    readFile(ItemsProvider.menuChangelogFilePath)
  }

  override def show(itemId: Int): String = {
    responseService.mkBeerResponse(itemId)._1
  }

  override def menuRaw(): String = {
    readFile(ItemsProvider.savedMenuFilePath)
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

  override def users(): String = {
    "Users:<br>" +
      db.users.foldLeft("")((s, u) => {
        s + (u.toString + "<br>")
      })
  }

  override def getUser(userId: Int): String = {
    db.getUser(userId).toString
  }

  private def readFile(filePath: String): String = {
    s"<pre>${FileUtils
      .readFile(filePath)
      .getOrElse(s"Can't read file '$filePath'")}</pre>"
  }
}
