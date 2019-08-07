package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models.{ChatId, InlineKeyboardMarkup, Message, ReplyMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.MenuTab
import org.duckofdoom.howardbot.utils.PaginationUtils
import slogging.StrictLogging

import scala.concurrent.Future
import scala.util.Try
import scala.util.matching.Regex

class HowardBot(val config: Config)(implicit responseService: ResponseService)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future]
    with Callbacks[Future] {

  override val client: RequestHandler[Future] = new CustomScalajHttpClient(config.token)

  private val menuPaginationRegex = (PaginationUtils.menuPaginationPrefix + "(\\d+)").r

  override def receiveMessage(msg: Message): Future[Unit] = {
    processShowRequest(msg) match {
      case Some((item, markup)) => {
        request(
          SendMessage(ChatId(msg.source),
                      item,
                      ParseMode.HTML.some,
                      true.some,
                      None,
                      None,
                      markup.some)
        ).void
//        respond(item, markup.some)(msg)
      }
      case _ => super.receiveMessage(msg)
    }
  }

  onCallbackQuery { implicit query =>
    // TODO: User atto for parsing
    def mkMenuResponse(page: Int, msg: Message, newMessage: Boolean) = {
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
      case (Some("menu"), Some(msg)) => mkMenuResponse(1, msg, newMessage = true).some
      case (Some(menuPaginationRegex(page)), Some(msg)) =>
        Try(page.toInt).toOption match {
          case Some(p) => mkMenuResponse(p, msg, newMessage = false).some
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

  // TODO: Move command literals to separate file
  onCommand("start" | "menu") { implicit msg =>
    val (items, markup) = responseService.mkMenuResponsePaginated(
      MenuTab.Bottled,
      1,
      config.menuItemsPerPage
    )

    respond(items, markup.some)
  }

  // TODO: Move command literals to separate file
  private def processShowRequest(msg: Message): Option[(String, InlineKeyboardMarkup)] = {
    val showRegex: Regex = "\\/show(\\d+)".r
    msg.text
      .fold(Option.empty[Int]) {
        case showRegex(id) => Try(id.toInt).toOption
        case _             => None
      }
      .fold(Option.empty[(String, InlineKeyboardMarkup)]) { s =>
        responseService.mkItemResponse(s).some
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
