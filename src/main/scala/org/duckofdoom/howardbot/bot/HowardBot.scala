package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models.{Chat, ChatId, InlineKeyboardMarkup, Message, ReplyMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.MenuTab
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User
import org.duckofdoom.howardbot.utils.PaginationUtils
import slogging.StrictLogging

import scala.concurrent.Future
import scala.util.Try
import scala.util.matching.Regex

class HowardBot(val config: Config)(implicit responseService: ResponseService, db: DB)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future]
    with Callbacks[Future] {

  type TelegramUser = com.bot4s.telegram.models.User

  // TODO: We need to serve multiple users in separate threads. Right now one user blocks everything =(
  override val client: RequestHandler[Future] = new CustomScalajHttpClient(config.token)

  private val menuPaginationRegex = (PaginationUtils.menuPaginationPrefix + "(\\d+)").r

  // TODO: Move command literals to separate file
  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkMenuResponsePaginated(
        MenuTab.Bottled,
        u.state.menuPage,
        config.menuItemsPerPage
      )

      respond(items, markup.some)
    }
  }

  onCallbackQuery { implicit query =>
    withUser(query.from) { u =>

      def mkMenuResponse(page: Int, msg: Message, newMessage: Boolean) = {

        u.state.menuPage = page
        db.updateUser(u)

        val (items, buttons) = responseService
          .mkMenuResponsePaginated(
            MenuTab.Bottled,
            page,
            config.menuItemsPerPage
          )
        if (newMessage) {
          request(
            SendMessage(ChatId(msg.source),
              items,
              ParseMode.HTML.some,
              true.some,
              None,
              None,
              buttons.some)
          )
        } else {
          request(
            EditMessageText(ChatId(msg.source).some,
              msg.messageId.some,
              None,
              items,
              ParseMode.HTML.some,
              true.some,
              buttons.some)
          )
        }
      }

      val responseFuture = (query.data, query.message) match {
        case (Some("menu"), Some(msg)) =>
          mkMenuResponse(u.state.menuPage, msg, newMessage = true).some
        case (Some(menuPaginationRegex(page)), Some(msg)) =>
          Try(page.toInt).toOption match {
            case Some(p) =>
              u.state.menuPage = p
              mkMenuResponse(p, msg, newMessage = false).some
            case _ =>
              logger.error(s"Failed to parse page from callback query data: ${query.data}")
              None
          }
        case _ => Future.successful().some
      }

      if (responseFuture.isEmpty)
        logger.error(s"Failed to construct response for callback query: ${query.data}")

      for {
        ack    <- ackCallback()
        answer <- responseFuture.getOrElse(Future.successful())
      } yield (ack, answer)
    }
  }

  override def receiveMessage(msg: Message): Future[Unit] = {

    val showRegex: Regex = "\\/show(\\d+)".r
    msg.text
      .fold(Option.empty[Int]) {
        case showRegex(id) => Try(id.toInt).toOption
        case _ => None
      }
      .fold(Option.empty[(String, InlineKeyboardMarkup)]) { s =>
        responseService.mkItemResponse(s).some
      } match {
      case Some((item, markup)) =>
        request(
          SendMessage(ChatId(msg.source),
            item,
            ParseMode.HTML.some,
            true.some,
            None,
            None,
            markup.some)
        ).void
      case _ => super.receiveMessage(msg)
    }
  }

  private def withUser(tgUser: TelegramUser)(action: User => Future[Unit]): Future[Unit] = {

    if (tgUser.firstName.isEmpty) {
      return Future.failed(new Exception(
        "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."))
    }

    val user = db.getUserByTelegramId(tgUser.id) match {
      case Some(u) => u.some
      case _ =>
        db.putUser(
          tgUser.id,
          tgUser.firstName,
          tgUser.lastName,
          tgUser.username
        )
    }

    user match {
      case Some(u) => action(u)
      case _ =>
        Future.failed(
          new Exception(
            s"Failed to process action! Could not create user for telegramUser: $tgUser"))
    }
  }

  private def withUser(chat: Chat)(action: User => Future[Unit]): Future[Unit] = {
    if (chat.firstName.isEmpty) {
      return Future.failed(new Exception(
        "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."))
    }

    val user = db.getUserByTelegramId(chat.id) match {
      case Some(u) => u.some
      case _ =>
        db.putUser(
          chat.id,
          chat.firstName.get,
          chat.lastName,
          chat.username
        )
    }

    user match {
      case Some(u) => action(u)
      case _ =>
        Future.failed(
          new Exception(s"Failed to process action! Could not create user for chat: $chat"))
    }
  }

  private def respond(text: String, markup: Option[ReplyMarkup] = None)(
      implicit message: Message) = {

    reply(
      text,
      ParseMode.HTML.some,
      true.some,
      None,
      None,
      markup,
    ).void
  }
}
