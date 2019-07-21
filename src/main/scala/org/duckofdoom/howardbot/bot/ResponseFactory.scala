package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.data.Item

trait ResponseFactory {
  def mkMenuResponse(menuItems: List[Item]): String
  def mkItemResponse(menuItem: Item): String
}

class PlaceholderResponseFactory extends ResponseFactory {
  
  override def mkMenuResponse(menuItems: List[Item]) : String = {
    val items = menuItems.foldLeft("")((s: String, i: Item) => s + s"\t${i.name} - ${i.price}")
    s"Вот такие вот у нас есть пивасики:\n$items"
  }
  
  override def mkItemResponse(item: Item) : String = {
    
    "Вот твой пивас:\n" +
    s"${item.name}\n\n" +
    s"Он стоит целых ${item.price} денег!\n" +
    s"Вот что можно про него сказать:\n\n${item.fullInfo}" 
  }
}
