package org.duckofdoom.howardbot.bot

import java.io.{PrintWriter, StringWriter}
import java.time.{Duration, LocalTime}

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{ItemDataProvider, PlaceholderItemDataProvider}
import slogging.StrictLogging

import scala.concurrent.{Await, Future}
import cats.syntax.option._

trait BotStatus {
  def runningTime: Duration 
  def restartCount: Int  
  def restartReason: Option[String] 
}

class BotStarter extends BotStatus
  with StrictLogging {
  
  // TODO: From LocalTime to LocalDateTime
  override def runningTime: Duration = Duration.between(startupTime, LocalTime.now())
  override def restartReason: Option[String] = lastRestartReason
  override def restartCount: Int = restarts

  private var startupTime: LocalTime = LocalTime.now()
  private var lastRestartReason: Option[String] = None
  private var restarts: Int = 0
  
  implicit val dataProvider: ItemDataProvider = new PlaceholderItemDataProvider
  implicit val responseService: ResponseService = new ResponseServiceImpl

  def run(implicit reloadConfig: () => Option[Config]): Future[Unit] = {
    try {
      startBot(reloadConfig)
    }
    catch {
      case e: Exception =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        val message = s"Caught exception while running bot:\n$sw\n Restarting..."
        logger.error(message)
        lastRestartReason = message.some
        restarts += 1
        run(reloadConfig)
    }
  }

  private def startBot(implicit reloadConfig: () => Option[Config]): Future[Unit] = {
    logger.info("Running bot.")

    reloadConfig() match {
      case None =>
        logger.error("Bot failed to start because config file is invalid!")
        lastRestartReason = "Invalid configuration!".some
        Future.unit
      case Some(conf) =>
        val bot = new HowardBot(conf)
        startupTime = LocalTime.now()
        Await.result(bot.run(), scala.concurrent.duration.Duration.Inf)
        bot.shutdown() // initiate shutdown
        Future.failed(new Exception("Bot was shut down"))
    }
  }
}
