package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.server.Server
import slogging._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends StrictLogging {
  
  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory()
    LoggerConfig.level = LogLevel.TRACE

    henloWorld()
    
    Server.run()
  }

  private def runBot(): Unit = {
    
    logger.info("Running bot.")

    // To run spawn the bot
    val bot = new RandomBot("TOKEN")
    val eol = bot.run()
    println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    bot.shutdown() // initiate shutdown
    // Wait for the bot end-of-life
    Await.result(eol, Duration.Inf)
  }
  

  private def henloWorld(): Unit = {
    
//    val config = BotConfig.createDefault()
//    
//    println(config)
//    val j = config.asJson.toString()
//    
//    println("Initial config:")
//    println(j)
//    
//    BotConfig.save(config)
//    println("Saved config...")
//    val conf = BotConfig.load()
//    
//    println(s"Loaded Config:\n$conf")
  }
}
