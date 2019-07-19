package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.untappd.MenuItem

object Responses {
  def createMenuResponse(menuItems: List[MenuItem]) : String = {
    val items = menuItems.foldLeft("")((s: String, i: MenuItem) => s + s"\t${i.name} - ${i.price}")
    s"This is the menu:\n$items"
  }
  
  def createMenuItemDescriptionResponse(item: MenuItem) : String = {
    s"This is the item:\n"
  }
}
