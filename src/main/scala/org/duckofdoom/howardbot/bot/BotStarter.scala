package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.utils.Extensions._
import java.time.{Duration, LocalDateTime}

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.services.ResponseService
import org.duckofdoom.howardbot.db.DB
import slogging.StrictLogging

import scala.concurrent.{Await, Future}

// TODO: Get rid of this =_=
class BotStarter(implicit responseService: ResponseService) extends StrictLogging {

  def runningTime: Duration         = Duration.between(startupTime, LocalDateTime.now())
  def restartReason: Option[String] = lastRestartReason
  def restartCount: Int             = restarts

  private var startupTime: LocalDateTime        = LocalDateTime.now()
  private var lastRestartReason: Option[String] = None
  private var restarts: Int                     = 0

  def run(implicit loadConfig: () => Option[Config], db: DB): Future[Unit] = {
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
    } catch {
      case e: Exception =>
        val message = s"Caught exception while running bot:\n${e.toStringFull}\n Restarting..."
        logger.error(message)
        lastRestartReason = message.some
        restarts += 1
        run(loadConfig, db)
    }
  }
}
