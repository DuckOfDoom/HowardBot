package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.ParseMode
import com.bot4s.telegram.models.Message
import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.Future
import scala.util.Try
import scala.util.matching.Regex

class HowardBot(val botConfig: Config)(implicit responseService: ResponseService)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future] {

  override val client: RequestHandler[Future] = new CustomScalajHttpClient(botConfig.token)

  override def receiveMessage(msg: Message): Future[Unit] = {
    processShowRequest(msg) match {
      case Some(response) => respond(response)(msg)
      case _              => super.receiveMessage(msg)
    }
  }

  // TODO: Move command literals to separate file
  onCommand("menu") { implicit msg =>
    respond(responseService.mkMenuResponse()).void
  }

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

}
