package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.server.Server
import slogging._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE
    
    // TODO: Maybe split config into two files so we don't have to load it two times?
    val server = Server.run(Config.load)
    val bot = Bot.run(() => Config.load)

    for {
      a <- server
      b <- bot
    } yield (a, b)

    logger.error("we're done here")
  }
}
