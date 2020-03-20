package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.utils.Extensions._
import java.time.{Duration, LocalDateTime}

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.services.ResponseService
import org.duckofdoom.howardbot.db.DB
import slogging.StrictLogging

import scala.concurrent.{Await, Future}

trait Bot {
  def sendNotification(userId:Int, title: String, message: String) : Future[Unit]
  def runningTime: Duration
  def restartReason: Option[String] 
  def restartCount: Int

  def run(implicit loadConfig: () => Option[Config]): Future[Unit]
}

/*
  Wrapper for a bot. 
  Provides an interface for other classes to use some of the bot functions, e.g. sending a message.
 */
class BotStarter(responseService: ResponseService, db: DB) extends Bot 
  with StrictLogging {

  override def runningTime: Duration         = Duration.between(startupTime, LocalDateTime.now())
  override def restartReason: Option[String] = lastRestartReason
  override def restartCount: Int             = restarts

  override def sendNotification(userId: Int, title: String, message: String): Future[Unit] = {
    if (bot.isEmpty) {
      return Future.failed(new RuntimeException("Bot is not started yet, so we can't send message now."))
    }
    
    bot.get.sendNotification(userId, title, message)
  }
  
  private var bot:Option[HowardBot] = Option.empty[HowardBot]
  private var startupTime: LocalDateTime        = LocalDateTime.now()
  private var lastRestartReason: Option[String] = None
  private var restarts: Int                     = 0

  override def run(implicit loadConfig: () => Option[Config]): Future[Unit] = {
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
          val instance = new HowardBot(conf, responseService, db)
          bot = instance.some
          startupTime = LocalDateTime.now()
          val eol = instance.run()
          Await.result(eol, scala.concurrent.duration.Duration.Inf)
          instance.shutdown() // initiate shutdown
          Future.failed(new Exception("Bot was shut down"))
      }
    } catch {
      case e: Exception =>
        val message = s"Caught exception while running bot:\n${e.toStringFull}\n Restarting..."
        logger.error(message)
        lastRestartReason = message.some
        restarts += 1
        run(loadConfig)
    }
  }
}
