package org.duckofdoom.howardbot

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser.decode

import scala.io.Source
import cats.syntax.option._
import slogging.StrictLogging

object BotConfig extends FileHandler with StrictLogging {
  val configPath = "config.json"

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

class FileHandler() extends StrictLogging {

  def readFile(implicit path: String): Option[String] = {
    try {
      val s = Source.fromFile(path)
      val str = s.mkString
      s.close()
      str.some
    }
    catch {
      case e: Exception => {
        logger.error(s"Failed to read file '$path'! Exception:\n${e.toString}")
        None
      }
    }
  }

  def writeFile(contents: String, path: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(path))
    pw.write(contents)
    pw.close()
  }
}
