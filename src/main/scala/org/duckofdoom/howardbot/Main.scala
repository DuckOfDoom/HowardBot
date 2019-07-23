package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.{BotStarter, ResponseService, ResponseServiceImpl}
import org.duckofdoom.howardbot.bot.data.{ItemDataProvider, PlaceholderItemDataProvider}
import org.duckofdoom.howardbot.server.{Server, ServerResponseService, ServerResponseServiceImpl}
import slogging._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE
    
    // TODO: Maybe split config into two files so we don't have to load it two times?
    implicit val configLoader: () => Option[Config] = () => Config.load
    implicit val config: Option[Config] = configLoader()

    implicit val dataProvider: ItemDataProvider = new PlaceholderItemDataProvider
    implicit val responseService: ResponseService = new ResponseServiceImpl
    implicit val bot: BotStarter = new BotStarter()
    implicit val serverResponseService: ServerResponseService = new ServerResponseServiceImpl()
    
    val server = new Server()

    for {
      a <- server.run
      b <- bot.run
    } yield (a, b)

    logger.error("we're done here")
  }
}
