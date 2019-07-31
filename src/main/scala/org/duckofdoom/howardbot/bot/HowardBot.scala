package org.duckofdoom.howardbot.bot

import cats.instances.future._
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

class HowardBot(val botConfig: Config)(implicit responseService: ResponseService)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future] {
  
  override val client: RequestHandler[Future] = new CustomScalajHttpClient(botConfig.token)

  override def receiveMessage(msg: Message): Future[Unit] = {
    // TODO: Add parsing for '/show' commands
      
    super.receiveMessage(msg)
  }

  // TODO: Move command literals to separate file
  onCommand("menu") { implicit msg =>
  
    reply(responseService.mkMenuResponse(), parseMode = Some(ParseMode.HTML)).void
  }

  onCommand("show") { implicit msg =>
    logger.info(s"Received message ${msg.text}")
    withArgs { args =>
      {
        if (args.isEmpty) {
          reply("Што? о_О").void
        } else {
          Try(args.head.toInt).toOption match {
            case Some(x) =>
              reply(responseService.mkItemResponse(x), parseMode = Some(ParseMode.HTML)).void
            case None =>
              reply(responseService.mkInvalidArgumentResponse(args.head),
                    parseMode = Some(ParseMode.HTML)).void
          }
        }
      }
    }
  }
}
