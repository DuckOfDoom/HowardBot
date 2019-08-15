package org.duckofdoom.howardbot.utils

import scala.util.Try

object Extractors {
  
  object Int {
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }
}
