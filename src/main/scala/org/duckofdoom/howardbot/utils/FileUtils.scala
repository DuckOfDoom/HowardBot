package org.duckofdoom.howardbot.utils

import java.io.FileNotFoundException

import cats.syntax.option._
import slogging.StrictLogging

/**
  * A class to handle file operations
  */
object FileUtils extends StrictLogging {

  /**
    * Reads file from supplied path
    */
  def readFile(implicit path: String): Option[String] = {
    import scala.io.Source

    try {
      val s = Source.fromFile(path)
      val str = s.mkString
      s.close()
      str.some
    }
    catch {
      case _: FileNotFoundException => None
      case e: Exception => 
        logger.error(s"Failed to read file '$path'! Exception:\n${e.toString}")
        None
    }
  }

  /**
    * Writes a file at supplied path
    */
  def writeFile(path: String, contents: String): Unit = {
    import java.io._
    
    val pw = new PrintWriter(new File(path))
    pw.write(contents)
    pw.close()
  }
}
