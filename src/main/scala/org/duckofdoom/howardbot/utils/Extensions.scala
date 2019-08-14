package org.duckofdoom.howardbot.utils

import java.io.{PrintWriter, StringWriter}

object Extensions {
  implicit class ThrowableExtensions(val e: Throwable) extends AnyVal {

    /**
      * Prints exception with a stack trace.
      */
    def toStringFull: String = {
      val sw = new StringWriter
      e.printStackTrace(new PrintWriter(sw))
      s"${e.getMessage}\n$sw"
    }
  }

}
