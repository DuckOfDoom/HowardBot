package org.duckofdoom.howardbot
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.ScalajHttpService
import slogging.MessageFormatter.PrefixFormatter
import slogging._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

class InvalidConfigurationException(msg: String) extends Exception(msg) {}

object Main {

  class LogFormatter extends PrefixFormatter {
    val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

    @inline
    private def getTimestamp = LocalDateTime.now.format(formatter)

    @inline
    private def getLevel(level: MessageLevel): String = {
      level match {
        case MessageLevel.trace => "TRACE"
        case MessageLevel.debug => "DEBUG"
        case MessageLevel.info  => "INFO"
        case MessageLevel.warn  => "WARN"
        case MessageLevel.error => "ERROR"
      }
    }

    override def formatPrefix(level: MessageLevel, name: String): String = {
      s"[$getTimestamp, ${getLevel(level)}, $name] "
    }
  }

  def main(args: Array[String]): Unit = {

    PrintLoggerFactory.formatter = new LogFormatter
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE

    new App()
//    test()
  }

  def test(): Unit = {
    val http = new ScalajHttpService

    implicit val ec: ExecutionContextExecutor = ExecutionContext.global

    val t = 25676
    val n = 98350

    val result = Await.result(
      http.makeRequestAsync(s"https://business.untappd.com/locations/$t/themes/$n/js"),
      Duration.Inf
    )

    print(MenuParser.parseMenu(result))
  }
}
