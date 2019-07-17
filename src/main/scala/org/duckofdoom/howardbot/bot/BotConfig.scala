package org.duckofdoom.howardbot.bot

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.duckofdoom.howardbot.utils.FileHandler
import slogging.StrictLogging

object BotConfig extends FileHandler with StrictLogging {
  val configPath = "config.json"

  def createDefault() : BotConfig = BotConfig("one", "two", InnerConfig(1,2))

  def load(implicit path: String = configPath): Option[BotConfig] = readFile(path).flatMap(f = s => {
    decode[BotConfig](s) match {
      case Right(c) => Some(c)
      case Left(err) => logger.error(s"Failed to parse bot config! Error: $err"); None
    }
  })

  def save(botConfig: BotConfig, path: String = configPath): Unit = {
    writeFile(botConfig.asJson.toString, path)
  }
}

case class BotConfig(A: String, B: String, inner: InnerConfig) {
}

case class InnerConfig(x: Int, y: Int) {
}

