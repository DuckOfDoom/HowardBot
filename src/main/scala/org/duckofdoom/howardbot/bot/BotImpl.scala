package org.duckofdoom.howardbot.bot

import java.time.{Duration, LocalDateTime}

import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.option._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models._
import org.duckofdoom.howardbot.bot.data.ItemType
import org.duckofdoom.howardbot.bot.services.ResponseService
import org.duckofdoom.howardbot.bot.utils.{Callback, ResponseFormat}
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User
import org.duckofdoom.howardbot.utils.Extensions._
import org.duckofdoom.howardbot.utils.Extractors._
import slogging.StrictLogging

import scala.concurrent.Future

class BotImpl(token: String, responseService: ResponseService, db: DB, howardBot: HowardBot)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future]
    with Callbacks[Future]
    with Bot {

  override def runningTime: Duration     = Duration.between(startupTime, LocalDateTime.now())
  private val startupTime: LocalDateTime = LocalDateTime.now()

  type TelegramUser = com.bot4s.telegram.models.User

  override val client: RequestHandler[Future] = new CustomScalajHttpClient(token)

  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { implicit u =>
      respond(howardBot.showMenu(), newMessage = true)
    }
  }

  onCommand("styles") { implicit msg =>
    withUser(msg.chat) { implicit u =>
      respond(howardBot.showStyles(), newMessage = true)
    }
  }

  onCallbackQuery { implicit query =>
    withUser(query.from) { implicit u =>
      val responseFuture = (query.data, query.message) match {
        case (Some(data), Some(msg)) =>
          implicit val message: Message = msg

          Callback.deserialize(data.getBytes) match {
            case Some(Callback.Menu(page)) =>
              respond(howardBot.showMenu(page), page.isEmpty).some
            case Some(Callback.Styles(page)) =>
              respond(howardBot.showStyles(page), page.isEmpty).some
            case Some(Callback.BeersByStyle(style, page)) =>
              respond(howardBot.showBeersByStyle(style, page), page.isEmpty).some
            case Some(Callback.SingleItem(itemType, itemId)) =>
              respond(howardBot.showItem(itemType, itemId), itemType == ItemType.Style).some
            case Some(Callback.Search(searchQuery, page)) =>
              respond(howardBot.search(searchQuery, page), newMessage = page.isEmpty).some
            case Some(Callback.Settings()) =>
              respond(howardBot.showSettings(), newMessage = true).some
            case Some(Callback.ChangeSorting(mSorting)) =>
              respond(howardBot.changeSorting(mSorting), newMessage = false).some
            case Some(Callback.ToggleNotifications()) =>
              respond(howardBot.toggleNotifications(), newMessage = false).some
            case _ =>
              None
          }
        case _ => None
      }

      if (responseFuture.isEmpty)
        logger.error(s"Failed to construct response for callback query: ${query.data}")

      // Combine our response with ACK since we should always acknowledge callback query
      val responseFutureWithAck: Future[Unit] = for {
        ack    <- ackCallback()
        answer <- responseFuture.getOrElse(Future.successful())
      } yield (ack, answer)

      responseFutureWithAck
    }
  }

  override def receiveMessage(msg: Message): Future[Unit] = {

    implicit val message: Message = msg

    if (msg.text.isEmpty) {
      logger.warn("Received empty text message.")
      return super.receiveMessage(msg)
    }

    withUser(msg.chat) { implicit u =>
      implicit val format: ResponseFormat.Value = ResponseFormat.TextMessage

      val responseFuture = msg.text.get match {
        case Consts.showItemRegex(Int(id)) =>
          respond(howardBot.showItem(ItemType.Beer, id), newMessage = true, allowWebPagePreview = true).void
        case Consts.showItemsByStyleRegex(Int(styleId)) =>
          respond(howardBot.showBeersByStyle(styleId, None), newMessage = true)
        case _ =>
          if (msg.text.get.startsWith("/")) {
            super.receiveMessage(msg)
          } else {
            respond(howardBot.search(msg.text.get, None), newMessage = true)
          }
      }

      responseFuture
    }
  }

  override def receiveUpdate(u: Update, botUser: Option[TelegramUser]): Future[Unit] = {
    try {
      super.receiveUpdate(u, botUser)
    } catch {
      case ex: Throwable =>
        logger.error(s"Caught exception when receiving update:\n${ex.toStringFull}")
        Future.failed(ex)
    }
  }

  def sendNotification(userId: Int, title: String, message: String): Future[Unit] = {
    request(
      SendMessage(
        ChatId(userId),
        responseService.formatNotification(title, message),
        parseMode = ParseMode.HTML.some
      )
    ).void
  }

  private def withUser[A](tgUser: TelegramUser)(action: User => A): A = {
    if (tgUser.firstName.isEmpty) {
      new Exception(
        "tgUser.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."
      )
    }

    val user = db.getUserByTelegramId(tgUser.id) match {
      case Some(u) => u
      case _ =>
        db.putUser(
          tgUser.id,
          tgUser.firstName,
          tgUser.lastName,
          tgUser.username
        )
    }

    action(user)
  }

  private def withUser[A](chat: Chat)(action: User => A): A = {
    if (chat.firstName.isEmpty) {
      throw new Exception(
        "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."
      )
    }

    val user = db.getUserByTelegramId(chat.id) match {
      case Some(u) => u
      case _ =>
        db.putUser(
          chat.id,
          chat.firstName.get,
          chat.lastName,
          chat.username
        )
    }

    action(user)
  }

  private def respond(response: (String, ReplyMarkup), newMessage: Boolean, allowWebPagePreview: Boolean = false)(
      implicit msg: Message
  ): Future[Unit] = {

    val (items, buttons) = response

    if (newMessage) {
      request(
        SendMessage(
          ChatId(msg.source),
          items,
          ParseMode.HTML.some,
          (!allowWebPagePreview).some,
          None,
          None,
          buttons.some
        )
      )
    } else {
      request(
        EditMessageText(
          ChatId(msg.source).some,
          msg.messageId.some,
          None,
          items,
          ParseMode.HTML.some,
          (!allowWebPagePreview).some,
          buttons.some
        )
      )
    }

    Future.unit
  }

}
