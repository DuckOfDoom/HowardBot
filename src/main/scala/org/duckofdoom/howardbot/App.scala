package org.duckofdoom.howardbot

import java.util.concurrent.Executors

import org.duckofdoom.howardbot.bot.data.{ItemsProvider, ParsedItemsProvider}
import org.duckofdoom.howardbot.bot.{
  BotStarter,
  ResponseService,
  ResponseServiceImpl,
  StatusProvider
}
import org.duckofdoom.howardbot.db.{DB, DoobieDB}
import org.duckofdoom.howardbot.server.{Server, ServerResponseService, ServerResponseServiceImpl}
import org.duckofdoom.howardbot.utils.{HttpService, ScalajHttpService}
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class App extends StrictLogging {

  implicit val configLoader: () => Option[Config] = () => Config.load
  implicit val mConfig: Option[Config]            = configLoader()

  if (mConfig.isEmpty)
    throw new InvalidConfigurationException("No valid config found!")

  implicit val config: Config = mConfig.get

  logger.info(
    s"Creating ExecutionContext with WorkStealingPool. Parallelism level = ${config.parallelismLevel}"
  )
  implicit val executionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newWorkStealingPool(config.parallelismLevel))

  implicit val httpService: HttpService                     = new ScalajHttpService
  implicit val dataProvider: ItemsProvider                  = new ParsedItemsProvider
  implicit val db: DB                                       = new DoobieDB(config.postgres)
  implicit val responseService: ResponseService             = new ResponseServiceImpl
  implicit val bot: BotStarter                              = new BotStarter()
  implicit val statusProvider: StatusProvider               = new StatusProvider()
  implicit val serverResponseService: ServerResponseService = new ServerResponseServiceImpl()
  val server                                                = new Server()

  Await.result(
    Future.sequence(
      Seq(dataProvider.startRefreshLoop, server.run, bot.run)
    ),
    Duration.Inf
  )
}
