package org.duckofdoom.howardbot.bot

import java.io.{PrintWriter, StringWriter}

import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import java.time._

object Bot extends StrictLogging {
  var startupTime : LocalTime = LocalTime.now()
  var restartCount : Int = 0

  def run(): Future[Unit] = {
    try {
      startBot()
    }
    catch {
      case e: Exception =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        logger.error(s"Caught exception while running bot:\n$sw\n Restarting...")
        restartCount += 1;
        run()
    }
  }

  def startBot(): Future[Unit] = {
    logger.info("Running bot.")

    val bot = new RandomBot("TOKEN")
    val eol = bot.run()
    
    startupTime = LocalTime.now()
    
    Await.result(eol, Duration.Inf)

    bot.shutdown() // initiate shutdown
    eol
  }
}
