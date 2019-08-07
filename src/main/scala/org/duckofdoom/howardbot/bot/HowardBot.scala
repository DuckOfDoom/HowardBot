package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageReplyMarkup, EditMessageText, ParseMode}
import com.bot4s.telegram.models.{ChatId, Message, ReplyMarkup}
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

  override def receiveMessage(msg: Message): Future[Unit] = {
    processShowRequest(msg) match {
      case Some(response) => respond(response)(msg)
      case _              => super.receiveMessage(msg)
    }
  }

  onCallbackQuery { implicit query =>
    logger.info("Received callback query: " + query)
    ackCallback().void

    for {
      data <- query.data
      m    <- query.message
      p <- (PaginationUtils.menuPaginationPrefix + "(\\d+)").r
        .findFirstIn(data)
        .map(s => Try(s.toInt).toOption)
    } yield
      responseService.mkMenuResponsePaginated(
        MenuTab.Bottled,
        p,
        config.menuItemsPerPage
      )

//    request(
//    EditMessageText(
//      ChatId(msg.source).some,
//      msg.messageId.some,
//      None,
//      items,
//      ParseMode.HTML.some,
//      None,
//      markup.some
//    ).void
//  )

    // TODO: User atto for parsing

    ackCallback().void
  }

  // TODO: Move command literals to separate file
  onCommand("menu") { implicit msg =>
    val (items, markup) = responseService.mkMenuResponsePaginated(
      MenuTab.Bottled,
      1,
      config.menuItemsPerPage
    )

    respond(items, markup.some)
  }

  // TODO: Move command literals to separate file
  private def processShowRequest(msg: Message): Option[String] = {
    val showRegex: Regex = "\\/show(\\d+)".r
    msg.text
      .fold(Option.empty[Int]) {
        case showRegex(id) => Try(id.toInt).toOption
        case _             => None
      }
      .fold(Option.empty[String]) { s =>
        responseService.mkItemResponse(s).some
      }
  }

  private def respond(text: String, markup: Option[ReplyMarkup] = None)(
      implicit message: Message) = {

    reply(
      text,
      ParseMode.HTML.some,
      None,
      None,
      None,
      markup,
    ).void
  }
}
