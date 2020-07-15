package org.duckofdoom.howardbot.server

import cats.syntax.option._
import org.duckofdoom.howardbot.bot.Consts
import org.duckofdoom.howardbot.bot.services.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.services.{ResponseFormat, ResponseService, StatusService}
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.utils.FileUtils
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.services.NotificationsService
import scalatags.Text.all._

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait ServerResponseService {
  // TODO: Convert methods without parameters to properties
  def home: String
  def menuAvailable: String
  def menuOnDeck: String
  def menuOutOfStock: String
  def menuFull: String
  def menuChangelog: String
  def menuRaw: String
  def styles: String
  def show(itemId: Int): String
  def notificationsForm: String
  def sendNotification(title: String, message: String, liveNotification: Boolean): String
  def users: String
  def getUser(userId: Int): String
  def putRandomUser(): String
}

class ServerResponseServiceImpl(
  statusService: StatusService,
  itemsProvider: ItemsProvider,
  responseService: ResponseService,
  notificationsService: NotificationsService,
  db: DB
) extends ServerResponseService {

  implicit val responseFormat: ResponseFormat = ResponseFormat.TextMessage

  override def home: String = {
    s"${statusService.getStatusInfoHtml}\n" +
      frag(
        p(a(href := "/users")("Users")),
        p(a(href := "/menu/available")("Menu [Available]")),
        p(a(href := "/menu/ondeck")("Menu [On Deck]")),
        p(a(href := "/menu/outofstock")("Menu [Out Of Stock]")),
        p(a(href := "/menu/full")("Menu [Full]")),
        p(a(href := "/menu/raw")("Menu [Raw]")),
        p(a(href := "/menu/changelog")("Menu [Changelog]")),
        p(a(href := "/menu/styles")("Styles")),
        br(),
        p(a(href := "/notifications")("Notifications"))
      ).render
  }

  override def menuAvailable: String = {
    mkMenuResponse(itemsProvider.availableBeers)
  }

  override def menuOnDeck: String = {
    mkMenuResponse(itemsProvider.beers.filter(_.isOnDeck))
  }

  override def menuOutOfStock: String = {
    mkMenuResponse(itemsProvider.beers.filter(!_.isInStock))
  }

  override def menuFull: String = {
    mkMenuResponse(itemsProvider.beers)
  }

  override def menuRaw: String = {
    readFile(ItemsProvider.savedMenuFilePath)
  }

  override def menuChangelog: String = {
    readFile(ItemsProvider.menuChangelogFilePath)
  }
  
  override def styles: String = {
    val shortStyles = itemsProvider.getAvailableStyles(true).map(_.name)
    val longStyles = itemsProvider.getAvailableStyles(false).map(_.name)

    val frags = ListBuffer[Frag]()
    val shownLongStyles = ListBuffer[String]()
    
    for (shortSt <- shortStyles){
      frags.append(frag(shortSt + ":"))
      frags.append(br())

      for (longSt <- longStyles.filter(st => st.contains(shortSt + " - "))) {
        frags.append(raw("\t" + longSt))
        frags.append(br())
        
        shownLongStyles.append(longSt)
      }
      
      frags.append(br())
    }
    
    for (longSt <- longStyles.filter(st => !shownLongStyles.contains(st))) {
      frags.append(frag(longSt))
      frags.append(br())
    }
    
    frag(frags).render
  }

  override def show(itemId: Int): String = {
    responseService.mkBeerResponse(itemId)._1
  }

  override def notificationsForm: String = {
    val instruction =
      frag(
        "Привет! Тут можно разослать сообщение всем пользователям бота, которые не отписались от рассылки!",
        br(),
        s"Вам надо зайти на ",
        a(href := "https://telegra.ph", "https://telegra.ph"),
        " оформить там пост (можно с картинками и прочим),",
        br(),
        "придумать заголовок и скопипастить ссылку в поля ниже."
      )

    frag(
      form(
        action := "/notifications/send",
        method := "get",
        instruction,
        br(),
        br(),
        "Заголовок:",
        br(),
        input(`type` := "text", name := "title"),
        br(),
        "Ссылка на telegra.ph:",
        br(),
        input(`type` := "text", name := "message"),
        br(),
        br(),
        "Разослать НЕ ТЕСТОВУЮ нотификацию:",
        input(`type` := "checkbox", name := "isLive"),
        br(),
        input(`type` := "submit", value := "Разослать!")
      )
    ).render
  }

  override def sendNotification(title: String, message: String, isLive: Boolean): String = {
    var msgs = Seq[String]()

    if (title.isBlank) {
      msgs = msgs :+ "Заполните заголовок!"
    }

    if (!message.contains("telegra.ph")) {
      msgs = msgs :+ "Ссылка должна вести на https://telegra.ph!"
    }

    if (msgs.nonEmpty) {
      msgs = "Не удалось разослать сообщение! И вот почему:" +: msgs
    } else {
      val f  = notificationsService.sendNotification(title, message, isLive)
      val rs = Await.result(f, Duration.Inf)
      msgs = rs
    }

    val frags = ListBuffer[Frag]()
    for (m <- msgs.toList) {
      frags.append(frag(m))
      frags.append(br())
    }

    frag(
      a("Послать еще!", href := "/notifications"),
      br(),
      br(),
      frags
    ).render
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

  override def users: String = {
    "Users:<br>" +
      db.users.foldLeft("")((s, u) => {
        s + (u.toString + "<br>")
      })
  }

  override def getUser(userId: Int): String = {
    db.getUser(userId).toString
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

  private def readFile(filePath: String): String = {
    s"<pre>${FileUtils
      .readFile(filePath)
      .getOrElse(s"Can't read file '$filePath'")}</pre>"
  }
}
