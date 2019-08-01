package org.duckofdoom.howardbot.bot

import java.io.{PrintWriter, StringWriter}
import java.time.{Duration, LocalDateTime}

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.{Await, Future}

trait BotStatus {
  def runningTime: Duration 
  def restartCount: Int  
  def restartReason: Option[String] 
}

class BotStarter(implicit responseService: ResponseService) extends BotStatus
  with StrictLogging {
  
  override def runningTime: Duration = Duration.between(startupTime, LocalDateTime.now())
  override def restartReason: Option[String] = lastRestartReason
  override def restartCount: Int = restarts

  private var startupTime: LocalDateTime = LocalDateTime.now()
  private var lastRestartReason: Option[String] = None
  private var restarts: Int = 0

  def run(implicit loadConfig: () => Option[Config]): Future[Unit] = {
    try {
      loadConfig() match {
        case None =>
          Future.failed(new Exception("Invalid bot configuration!"))
        case Some(conf) =>
          if (!conf.startBot) {
            logger.info("Skipping bot start.")
            return Future.successful()
          }
          
          logger.info("Running bot.")
          val bot = new HowardBot(conf)
          startupTime = LocalDateTime.now()
          val eol = bot.run()
          Await.result(eol, scala.concurrent.duration.Duration.Inf)
          bot.shutdown() // initiate shutdown
          Future.failed(new Exception("Bot was shut down"))
      }
    }
    catch {
      case e: Exception =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        val message = s"Caught exception while running bot:\n$sw\n Restarting..."
        logger.error(message)
        lastRestartReason = message.some
        restarts += 1
        run(loadConfig)
    }
  }

  private def startBot(config: Option[Config]): Future[Unit] = {

    config match {
      case None =>
        logger.error("Bot failed to start because config file is invalid!")
        lastRestartReason = "Invalid configuration!".some
        Future.unit
      case Some(conf) =>
        val bot = new HowardBot(conf)
        startupTime = LocalDateTime.now()
        Await.result(bot.run(), scala.concurrent.duration.Duration.Inf)
        bot.shutdown() // initiate shutdown
        Future.failed(new Exception("Bot was shut down"))
    }
  }
}
