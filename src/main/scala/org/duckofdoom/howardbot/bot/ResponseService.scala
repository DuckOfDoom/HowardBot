package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._

trait ResponseService {
  def mkMenuResponse(): String
  def mkItemResponse(itemId: String) : String
  def mkInvalidArgumentResponse(value: String) : String
  def mkItemNotFoundResponse(value: String ): String 
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider, responseFactory: ResponseFactory) extends ResponseService {
  
  override def mkMenuResponse(): String = {
    div(
      h1("Меню:"),
      itemDataProvider.allItems.map(mkItemInfo).foldLeft("",
    )
  }
  
  override def mkItemResponse(itemId: Int): String = {
    itemDataProvider(itemId) match {
      case Some(item) =>
      case None => s"Позиции с id '$itemId' не существует."
    }
  }

  override def mkInvalidArgumentResponse(value: String ): String = {
    s"Неверный параметр: '$value'"
  }
  
  override def mkItemNotFoundResponse(value: String ): String = {
    s"Несуществующий предмет: '$value'"
  }
  
  private def mkItemInfo(item:Item): ConcreteHtmlTag[String] ={
    div(
      a(href(s"/show ${item.id}"), b(item.name)),
      br(),
      i(item.price) + "\u20BD",
      
  }
} 
