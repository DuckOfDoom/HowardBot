package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.data.MenuTab.MenuTab
import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._
import cats.syntax.option._
import scala.collection.mutable.ListBuffer

trait ResponseService {
  val defaultSeparator: String = ""

  def mkMenuResponse(itemLinkSeparator: String = defaultSeparator): String
  def mkMenuResponsePaginated(menuTab: MenuTab,
                              page: Int,
                              itemsPerPage: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int, itemLinkSeparator: String = defaultSeparator): String
  def mkInvalidArgumentResponse(value: String): String
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider) extends ResponseService {

  override def mkMenuResponse(itemLinkSeparator: String): String = {
    frag(
      itemDataProvider.allItems
        .map(i => mkItemInfo(i, inMenu = true, itemLinkSeparator))
        .toArray: _*
    ).render
  }

  override def mkMenuResponsePaginated(menuTab: MenuTab,
                                       page: Int,
                                       itemsPerPage: Int): (String, InlineKeyboardMarkup) = {

    var items      = itemDataProvider.getItems(menuTab)
    val totalPages = items.length / itemsPerPage

    if (items.length > itemsPerPage) {
      items = items.slice(page * itemsPerPage, page * itemsPerPage + itemsPerPage)
    }

    var buttons: ListBuffer[InlineKeyboardButton] = ListBuffer[InlineKeyboardButton]()

//    if (totalPages > 3) {
//      if (page > 2) {
//        buttons += InlineKeyboardButton("<< 1", "1".some)
//        buttons += InlineKeyboardButton(s"< ${page - 1}", (page - 1).toString.some)
//        buttons += InlineKeyboardButton(s"* $page *", page.toString.some)
//        buttons += InlineKeyboardButton(s"${page + 1} >", page.toString.some)
//        buttons += InlineKeyboardButton("<< 1", "1".some)
//      } else {
//        buttons += InlineKeyboardButton("<< 1", "1".some)
//        buttons += InlineKeyboardButton(s"< ${page - 1}", (page - 1).toString.some)
//        buttons += InlineKeyboardButton(s"* $page *", page.toString.some)
//        buttons += InlineKeyboardButton(s"${page + 1} >", page.toString.some)
//        buttons += InlineKeyboardButton("<< 1", "1".some)
//      }
//    } else {
//      page match {
//        case 1 =>
//          buttons += InlineKeyboardButton("* 1 *", "1".some)
//          buttons += InlineKeyboardButton("2 >", "2".some)
//          buttons += InlineKeyboardButton("3 >>", "3".some)
//        case 2 =>
//          buttons += InlineKeyboardButton("< 1", "1".some)
//          buttons += InlineKeyboardButton("* 2 *", "2".some)
//          buttons += InlineKeyboardButton("3 >", "3".some)
//        case 3 =>
//          buttons += InlineKeyboardButton("<< 1", "1".some)
//          buttons += InlineKeyboardButton("< 2", "2".some)
//          buttons += InlineKeyboardButton("* 3 *", "3".some)
//      }
//    }

    (
      frag(
        itemDataProvider.allItems
          .map(i => mkItemInfo(i, inMenu = true, " "))
          .toArray: _*
      ).render,
      InlineKeyboardMarkup.singleRow(buttons)
    )
  }

  override def mkItemResponse(itemId: Int, itemLinkSeparator: String): String = {
    itemDataProvider.getItem(itemId) match {
      case Some(item) =>
        mkItemInfo(item, inMenu = false, itemLinkSeparator).render // TODO: Separate response for a single item?
      case None => mkItemNotFoundResponse(itemId)
    }
  }

  override def mkInvalidArgumentResponse(value: String): String = {
    s"Неверный параметр: '$value'"
  }

  private def mkItemNotFoundResponse(itemId: Int): String = {
    s"Позиции с ID '$itemId' не существует."
  }

  private def mkItemInfo(item: Item, inMenu: Boolean, itemLinkSeparator: String) = {
    frag(
      b(item.name),
      "\n",
      s"Стиль: ${item.style.getOrElse("UNKNOWN")}",
      "\n",
      s"Пивоварня: ${item.breweryInfo.toString}",
      "\n",
      i(item.price.toString),
      "\n",
      if (inMenu)
//        a(href := s"/show$itemLinkSeparator${item.id}")("Подробнее...")
        s"/show$itemLinkSeparator${item.menuOrder}"
      else
        s"\n${item.description}",
      "\n\n"
    )
  }
}
