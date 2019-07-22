package org.duckofdoom.howardbot.bot.data

trait Item {
  val id : Int
  val name : String
  val style: String
  val price: Int
  val brewery : String
  val flavorText : String
}

/*
A placeholder menu item until we get api access
 */
case class PlaceholderItem(id: Int, name: String, style:String, price: Int, brewery:String, flavorText:String) extends Item {
}
