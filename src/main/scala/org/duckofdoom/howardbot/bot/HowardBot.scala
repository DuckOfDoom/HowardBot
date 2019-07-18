package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.ParseMode
import cats.instances.future._
import cats.syntax.functor._
import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.Future

class HowardBot(val botConfig: Config)
  extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future] {

  override val client: RequestHandler[Future] = new ScalajHttpClient(botConfig.token)

  onCommand("herp") { implicit msg => {
    reply("derp", parseMode = Some(ParseMode.HTML)).void
  }
  }
}
