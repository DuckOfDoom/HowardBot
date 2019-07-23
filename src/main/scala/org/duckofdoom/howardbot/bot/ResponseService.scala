package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._

trait ResponseService {
  val defaultSeparator : String = " " 
  
  def mkMenuResponse(itemLinkSeparator:String = defaultSeparator): String
  def mkItemResponse(itemId: Int, itemLinkSeparator:String = defaultSeparator) : String
  def mkInvalidArgumentResponse(value: String) : String
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider) extends ResponseService {
  
  override def mkMenuResponse(itemLinkSeparator:String): String = {
    frag(
        itemDataProvider
          .allItems
          .map(i => mkItemInfo(i, inMenu = true, itemLinkSeparator))
          .toArray:_*
    ).render
  }
  
  override def mkItemResponse(itemId: Int, itemLinkSeparator: String): String = {
    itemDataProvider.getItem(itemId) match {
      case Some(item) => mkItemInfo(item, inMenu = false, itemLinkSeparator).render // TODO: Separate response for a single item?
      case None => mkItemNotFoundResponse(itemId)
    }
  }

  override def mkInvalidArgumentResponse(value: String ): String = {
    s"Неверный параметр: '$value'"
  }

  private def mkItemNotFoundResponse(itemId: Int): String = {
    s"Позиции с ID '$itemId' не существует."
  }

  private def mkItemInfo(item:Item, inMenu:Boolean, itemLinkSeparator:String) = {
    frag(
        a(href := s"/show$itemLinkSeparator${item.id}")(b(item.name)),
        item.style,
        item.brewery,
        i(item.price + "\u20BD"),
        if (inMenu) "" else item.flavorText
    )
  }
} 
