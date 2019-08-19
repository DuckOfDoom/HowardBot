package org.duckofdoom.howardbot.utils

import java.io.{PrintWriter, StringWriter}

object Extensions {
  implicit class AnyRefExtensions[A](val o: A) extends AnyVal {

    def check(condition: A => Boolean, action: A => Unit): A = {
      if (!condition(o))
        action
      o
    }
  }

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

  implicit class IntExtensions(val i: Int) extends AnyVal {

    /**
      * Clamps value between two values
      */
    def clamp(from: Int, to: Int): Int = {
      if (from < to) {
        if (i < from)
          return from
        if (i > to)
          return to
      }
      else if (to < from) {
        if (i < to)
          return to
        if (i > from)
          return from
      }

      i
    }
  }
}
