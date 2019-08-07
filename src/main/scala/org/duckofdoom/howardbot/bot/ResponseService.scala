package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.data.MenuTab.MenuTab
import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._
import cats.syntax.option._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import slogging.StrictLogging

trait ResponseService {
  val defaultSeparator: String = ""

  def mkMenuResponse(itemLinkSeparator: String = defaultSeparator): String
  def mkMenuResponsePaginated(menuTab: MenuTab,
                              page: Int,
                              itemsPerPage: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int, itemLinkSeparator: String = defaultSeparator): String
  def mkInvalidArgumentResponse(value: String): String
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider)
    extends ResponseService
    with StrictLogging {

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

    var items = (0 to 100).map(i => { "item " + i }) //-itemDataProvider.allItems.toList
//    var items      = itemDataProvider.getItems(menuTab)
    val totalPages = items.length / itemsPerPage

    if (items.length > itemsPerPage) {
      items = items.slice((page - 1) * itemsPerPage, (page - 1) * itemsPerPage + itemsPerPage)
    }

    val renderedItems = frag(
      items.map(is => is + "\n")
//        .map(i => mkItemInfo(i, inMenu = true, " "))
//        .toArray: _*
    ).render

    logger.info(
      s"Getting items. Page:$page, itemsPerPage:$itemsPerPage, total items: ${items.length}, total pages: $totalPages")

    val buttons = PaginationUtils.mkButtonsForPaginatedQuery(page, totalPages, items.length)
    val markup = InlineKeyboardMarkup.singleRow(
      buttons.map {
        case Button(bText, bCallbackData) => InlineKeyboardButton.callbackData(bText, bCallbackData)
      }
    )

    (renderedItems, markup)
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
