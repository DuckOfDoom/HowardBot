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
    implicit val configLoader: () => Option[Config] = () => Config.load
    implicit val config: Option[Config] = configLoader()
    
    implicit val bot: Bot = new Bot()
    val server = new Server()

    for {
      a <- server.run
      b <- bot.run
    } yield (a, b)

    logger.error("we're done here")
  }
}
