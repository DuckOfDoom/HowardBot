package org.duckofdoom.howardbot
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import slogging.MessageFormatter.PrefixFormatter
import slogging._

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
  }
}
