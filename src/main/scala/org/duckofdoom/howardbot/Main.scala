package org.duckofdoom.howardbot
import slogging._

class InvalidConfigurationException(msg:String) extends Exception(msg) {}

object Main {

  def main(args: Array[String]): Unit = {
    
    LoggerConfig.factory = PrintLoggerFactory
    LoggerConfig.level = LogLevel.TRACE
    
    new App()    
  }
}
