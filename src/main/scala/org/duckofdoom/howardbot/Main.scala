package org.duckofdoom.howardbot

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.server.Server
import slogging._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StrictLogging {
  
  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE
    
    val server = Server.run()
    val bot = Bot.run()
    
    for {
      a <- server
      b <- bot
    } yield (a, b)
    
    logger.error("we're done here")
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
