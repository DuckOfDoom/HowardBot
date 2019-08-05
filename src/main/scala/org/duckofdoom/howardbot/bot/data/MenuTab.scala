package org.duckofdoom.howardbot.bot.data

/**
  *Menu tab for choosing appropriate item category
  */
object MenuTab extends Enumeration {
  type MenuTab = Value
  case object OnTap
  case object Bottled
  case object OnDeck
  case object Other
}
