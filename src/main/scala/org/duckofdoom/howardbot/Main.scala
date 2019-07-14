package org.duckofdoom.howardbot

import com.bot4s.telegram.future.TelegramBot

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import slogging._

object Main extends StrictLogging {
  def main(args: Array[String]): Unit = {

    LoggerConfig.factory = PrintLoggerFactory()
    LoggerConfig.level = LogLevel.DEBUG

    logger.error("s")
    
    // To run spawn the bot
    val bot = new RandomBot("249399445:AAHX9cG3MCsF9kJ9Y-c-LmAF3_RCALrIG2s")
    val eol = bot.run()
    println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    bot.shutdown() // initiate shutdown
    // Wait for the bot end-of-life
    Await.result(eol, Duration.Inf)
  }
}
