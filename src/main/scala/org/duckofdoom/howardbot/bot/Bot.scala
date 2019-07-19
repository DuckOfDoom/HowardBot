package org.duckofdoom.howardbot.bot

import java.io.{PrintWriter, StringWriter}
import java.time.LocalTime

import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import cats.syntax.option._

object Bot extends StrictLogging {
  var startupTime: LocalTime = LocalTime.now()
  var restartCount: Int = 0
  var lastRestartReason: Option[String] = None

  def run(implicit reloadConfig:() => Option[Config]): Future[Unit] = {
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
        restartCount += 1
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
        Await.result(bot.run(), Duration.Inf)
        bot.shutdown() // initiate shutdown
        Future.failed(new Exception("Bot was shut down"))
    }
  }
}
