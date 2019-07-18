package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.server.Server
import slogging._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE
    
    val server = Server.run()
    val bot = Bot.run()

    for {
      a <- server
      b <- bot
    } yield (a, b)

    logger.error("we're done here")
  }
}
