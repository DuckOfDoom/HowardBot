package org.duckofdoom.howardbot.bot

import scala.util.matching.Regex
import cats.syntax.option._

object Consts {

  val showItemPrefix = "/beer"
  val showStylePrefix = "/style"
  
  val showItemRegex: Regex = (showItemPrefix + "(\\d+)").r
  val showItemsByStyleRegex: Regex = (showStylePrefix + "(\\d+)").r
  
  val zwj = "&#8205;"

  object SearchBeerByNameQuery {

    def unapply(msg : String): Option[String] = {
      if (msg.startsWith("/b "))
        msg.substring("/b ".length).some
      else if (msg.startsWith("/beer "))
        msg.substring("/beer ".length).some
      else
        None
    }
  }
  
  object SearchBeerByStyleQuery {
    
    def unapply(msg : String): Option[String] = {
      if (msg.startsWith("/s "))
        msg.substring("/s ".length).some
      else if (msg.startsWith("/style "))
        msg.substring("/style ".length).some
      else
        None
    }
  }
  
}

