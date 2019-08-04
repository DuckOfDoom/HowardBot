package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.{BotStarter, ResponseService, ResponseServiceImpl}
import org.duckofdoom.howardbot.bot.data.{ItemDataProvider, FakeItemDataProvider}
import org.duckofdoom.howardbot.db.{DB, DoobieDB}
import org.duckofdoom.howardbot.server.{Server, ServerResponseService, ServerResponseServiceImpl}
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

class App extends StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  implicit val configLoader: () => Option[Config] = () => Config.load
  implicit val config: Option[Config]             = configLoader()

  if (config.isEmpty)
    throw new InvalidConfigurationException("No valid config found!")

  implicit val dataProvider: ItemDataProvider   = new FakeItemDataProvider
  implicit val db: DB                           = new DoobieDB(config.get.postgres)
  implicit val responseService: ResponseService = new ResponseServiceImpl
  implicit val bot: BotStarter                  = new BotStarter()

  implicit val serverResponseService: ServerResponseService = new ServerResponseServiceImpl()
  val server                                                = new Server()

  for {
    a <- server.run
    b <- bot.run
  } yield (a, b)
}
