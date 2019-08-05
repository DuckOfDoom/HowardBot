package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.ParseMode
import com.bot4s.telegram.models.{InlineKeyboardMarkup, Message}
import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.Future
import scala.util.Try
import scala.util.matching.Regex

class HowardBot(val botConfig: Config)(implicit responseService: ResponseService)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future] 
    with Callbacks[Future] {

  override val client: RequestHandler[Future] = new CustomScalajHttpClient(botConfig.token)

  override def receiveMessage(msg: Message): Future[Unit] = {
    processShowRequest(msg) match {
      case Some(response) => respond(response)(msg)
      case _              => super.receiveMessage(msg)
    }
  }

//  onCallbackQuery { implicit query =>
//    query.chatInstance
//    ackCallback()
//  }

  // TODO: Move command literals to separate file
  onCommand("menu") { implicit msg =>
    respond(responseService.mkMenuResponse()).void
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

  private def respond(text: String)(implicit message: Message) = {
    reply(text, parseMode = ParseMode.HTML.some).void
  }
//  
//  private def respondWithKeyboard(text: String, replyKeyboardMarkup: InlineKeyboardMarkup)(implicit message: Message) = {
//    reply(
//      text,
//      ParseMode.HTML.some,
//      None,
//      None, 
//      None,
//      replyKeyboardMarkup(),
//    ).void
//  }

}
