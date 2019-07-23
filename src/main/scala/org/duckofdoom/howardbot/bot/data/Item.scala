package org.duckofdoom.howardbot.bot.data

trait Item {
  val id: Int
  val name: String
  val style: String
  val brewery: String
  val price: Int
  val flavorText: String
}

/*
A placeholder menu item until we get api access
 */
case class PlaceholderItem(id: Int,
                           name: String,
                           style: String,
                           brewery: String,
                           price: Int,
                           flavorText: String)
    extends Item {}
