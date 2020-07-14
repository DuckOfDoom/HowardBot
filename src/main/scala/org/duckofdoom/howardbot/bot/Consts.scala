package org.duckofdoom.howardbot.bot

import scala.util.matching.Regex

object Consts {
  val showItemPrefix = "/beer"
  val showStylePrefix = "/style"
  
  val showItemRegex: Regex = (showItemPrefix + "(\\d+)").r
  val showItemsByStyleRegex: Regex = (showStylePrefix + "(\\d+)").r
  
  val zwj = "&#8205;"
}

