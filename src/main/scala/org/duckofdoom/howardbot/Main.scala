package org.duckofdoom.howardbot

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import slogging._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends StrictLogging {
  def main(args: Array[String]): Unit = {
    LoggerConfig.factory = PrintLoggerFactory()
    LoggerConfig.level = LogLevel.TRACE

    henloWorld()
    
//    runBot()
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
    
    val config = new BotConfig("herpj", "derperino")
    
    println(config)
    println()

    implicit val fooDecoder: Decoder[BotConfig] = deriveDecoder
    implicit val fooEncoder: Encoder[BotConfig] = new Encoder[BotConfig] {
      override def apply(a: BotConfig): Json = Json.obj(
        ("herp", Json.fromString("derp")),
        ("1", Json.fromInt(2))
      )
    }
    
    val j = config.asJson
    println(j)
    println()

    val decoded = j.as[BotConfig]
    
    println(decoded)
  }
}
