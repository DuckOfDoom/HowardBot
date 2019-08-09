package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.data.{ItemDataProvider, ParsedItemsDataProvider}
import org.duckofdoom.howardbot.bot.{
  BotStarter,
  ResponseService,
  ResponseServiceImpl,
  StatusProvider
}
import org.duckofdoom.howardbot.db.{DB, DoobieDB}
import org.duckofdoom.howardbot.server.{Server, ServerResponseService, ServerResponseServiceImpl}
import org.duckofdoom.howardbot.utils.ScalajHttpService
import slogging.StrictLogging

import scala.concurrent.ExecutionContext

class App extends StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  implicit val configLoader: () => Option[Config] = () => Config.load
  implicit val config: Option[Config]             = configLoader()

  if (config.isEmpty)
    throw new InvalidConfigurationException("No valid config found!")

  implicit val dataProvider: ItemDataProvider = new ParsedItemsDataProvider(
    new ScalajHttpService(),
    config.get
  )

  implicit val db: DB                                       = new DoobieDB(config.get.postgres)
  implicit val responseService: ResponseService             = new ResponseServiceImpl
  implicit val bot: BotStarter                              = new BotStarter()
  implicit val statusProvider: StatusProvider           = new StatusProvider()
  implicit val serverResponseService: ServerResponseService = new ServerResponseServiceImpl()
  val server                                                = new Server()

  dataProvider.refresh
  server.run
  bot.run
}
