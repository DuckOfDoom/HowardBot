package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text
import scalatags.Text.all._

import scala.collection.mutable.ListBuffer

trait ResponseService {
  def mkMenuResponse(): String
  def mkItemResponse(itemId: Int) : String
  def mkInvalidArgumentResponse(value: String) : String
  def mkItemNotFoundResponse(value: String ): String 
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider) extends ResponseService {
  
  override def mkMenuResponse(): String = {
//    var tags = Array
////    tags += h("Меню:")
////    tags += br()
////      tags += itemDataProvider.allItems.map(i => mkItemInfo(i, short = true)).toArray
//    
//    div(tags).render
  }
  
  override def mkItemResponse(itemId: Int): String = {
    itemDataProvider.getItem(itemId) match {
      case Some(item) => mkItemInfo(item, short = false).render // TODO: Separate response for a single item?
      case None => s"Позиции с id '$itemId' не существует."
    }
  }

  override def mkInvalidArgumentResponse(value: String ): String = {
    s"Неверный параметр: '$value'"
  }
  
  override def mkItemNotFoundResponse(value: String ): String = {
    s"Несуществующий предмет: '$value'"
  }
  
  private def mkItemInfo(item:Item, short:Boolean): ConcreteHtmlTag[String] ={
    div(
      a(href(s"/show ${item.id}"), b(item.name)),
      item.style,
      item.brewery,
      i(item.price + "\u20BD"),
      br(),
      if (short) "" else item.flavorText
    )
  }
} 
