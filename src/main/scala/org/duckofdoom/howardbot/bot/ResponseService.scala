package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._

trait ResponseService {
  def mkMenuResponse(): String
  def mkItemResponse(itemId: Int) : String
  def mkInvalidArgumentResponse(value: String) : String
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider) extends ResponseService {
  
  override def mkMenuResponse(): String = {
    frag(
        itemDataProvider
          .allItems
          .map(i => mkItemInfo(i, short = true))
          .toArray:_*
    ).render
  }
  
  override def mkItemResponse(itemId: Int): String = {
    itemDataProvider.getItem(itemId) match {
      case Some(item) => mkItemInfo(item, short = false).render // TODO: Separate response for a single item?
      case None => mkItemNotFoundResponse(itemId)
    }
  }

  override def mkInvalidArgumentResponse(value: String ): String = {
    s"Неверный параметр: '$value'"
  }

  private def mkItemNotFoundResponse(itemId: Int): String = {
    s"Позиции с ID '$itemId' не существует."
  }

  private def mkItemInfo(item:Item, short:Boolean) = {
    frag(
        a(href := s"/show ${item.id}")(b(item.name)),
        item.style,
        item.brewery,
        i(item.price + "\u20BD"),
        if (short) "" else item.flavorText
    )
  }
} 
