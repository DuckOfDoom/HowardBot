package org.duckofdoom.howardbot.bot.data

/**
  *Menu tab for choosing appropriate item category
  */
object MenuTab extends Enumeration {
  type MenuTab = Value
  val OnTap = Value(1)
  val Bottled = Value(1 << 1)
  val OnDeck = Value(1 << 2)
  val Other = Value(1 << 3)
}
