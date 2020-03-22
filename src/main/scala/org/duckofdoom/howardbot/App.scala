package org.duckofdoom.howardbot

import java.util.concurrent.Executors

import org.duckofdoom.howardbot.bot.data.{ItemsProvider, ItemsProviderImpl}
import org.duckofdoom.howardbot.bot.services._
import org.duckofdoom.howardbot.bot.{Bot, BotStarter}
import org.duckofdoom.howardbot.db.{DB, DoobieDB}
import org.duckofdoom.howardbot.server.{Server, ServerResponseService, ServerResponseServiceImpl}
import org.duckofdoom.howardbot.services.{HttpService, NotificationsService, ScalajHttpService}
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class App extends StrictLogging {

  implicit val mConfig: Option[Config] = Config.load
  if (mConfig.isEmpty)
    throw new InvalidConfigurationException("No valid config found!")

  implicit val config: Config = mConfig.get

  logger.info(
    s"Creating ExecutionContext with WorkStealingPool. Parallelism level = ${config.parallelismLevel}"
  )

  implicit val executionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newWorkStealingPool(config.parallelismLevel))

  val httpService: HttpService       = new ScalajHttpService
  val db: DB                         = new DoobieDB(config.postgres)
  val keyboardHelper: KeyboardHelper = new KeyboardHelperImpl()

  val itemsProvider: ItemsProvider = new ItemsProviderImpl
  val menuRefreshService: MenuRefreshService = new MenuRefreshServiceImpl(
    config.menuRefreshPeriod,
    config.mainMenuUrl,
    config.additionalPagesCount,
    page => config.getAdditionalResultPageUrl(page),
    config.httpRequestTimeout,
    itemsProvider,
    httpService
  )

  val responseHelper: ResponseHelper = new ResponseHelperImpl(
    config.stylesPerPage,
    config.menuItemsPerPage,
    itemsProvider,
    keyboardHelper
  )
  val responseService: ResponseService = new ResponseServiceImpl(
    itemsProvider,
    responseHelper,
    keyboardHelper
  )

  val bot: Bot = new BotStarter(responseService, db)

  val notificationsService = new NotificationsService(
    config.testNotificationsUserIds.toSet,
    config.menuUpdatesNotificationsUserIds.toSet,
    db,
    bot
  )

  val serverResponseService: ServerResponseService = new ServerResponseServiceImpl(
    new StatusService(bot, itemsProvider),
    itemsProvider,
    responseService,
    notificationsService,
    db
  )

  val server = new Server(serverResponseService)

  Await.result(
    Future.sequence(
      Seq(
        menuRefreshService.startRefreshLoop(
          changelog => notificationsService.sendMenuUpdates(changelog)
        ),
        server.run,
        bot.run
      )
    ),
    Duration.Inf
  )
}
