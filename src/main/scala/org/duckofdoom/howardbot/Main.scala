package org.duckofdoom.howardbot

import com.bot4s.telegram.future.TelegramBot

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import slogging._

object Main extends StrictLogging {
  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory()
    LoggerConfig.level = LogLevel.TRACE

    henloWorld()

  }

  private def runBot(): Unit = {
    logger.info("Running bot.")

    // To run spawn the bot
    val bot = new RandomBot("249399445:AAHX9cG3MCsF9kJ9Y-c-LmAF3_RCALrIG2s")
    val eol = bot.run()
    println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    bot.shutdown() // initiate shutdown
    // Wait for the bot end-of-life
    Await.result(eol, Duration.Inf)
  }

  private def henloWorld(): Unit = {

    val l = List(1, "ad", true)

    println(l)
    println(l.productArity)
    
    val d1 = new derp1(1, "1")
    val d2 = new derp2(2, "2")
    val d22 = new derp2(3, "2")
    
    println(d2.equals(d22))
  }

  class derp1(x: Int, s: String) {

  }
  
  case class derp2(x: Int, s: String) {

  }
}
