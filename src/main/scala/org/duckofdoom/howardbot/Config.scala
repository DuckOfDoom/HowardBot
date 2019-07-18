package org.duckofdoom.howardbot

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.duckofdoom.howardbot.utils.FileHandler
import slogging.StrictLogging

object Config extends FileHandler with StrictLogging {
  val configPath = "config.json"

  def load(implicit path: String = configPath): Option[Config] = readFile(path).flatMap(f = s => {
    decode[Config](s) match {
      case Right(c) => Some(c)
      case Left(err) => logger.error(s"Failed to parse bot config! Error: $err"); None
    }
  })

  def save(botConfig: Config, path: String = configPath): Unit = {
    writeFile(botConfig.asJson.toString, path)
  }
}

case class Config
(
  token: String,
  inner: InnerConfig
) 

case class InnerConfig(x: Int, y: Int)

