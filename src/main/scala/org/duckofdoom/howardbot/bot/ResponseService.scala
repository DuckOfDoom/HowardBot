package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.data.MenuTab.MenuTab
import org.duckofdoom.howardbot.bot.data.{Item, ItemDataProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import slogging.StrictLogging

trait ResponseService {
  val defaultSeparator: String = ""

  @deprecated("Move to ServerResponseService")
  def mkMenuResponse(itemLinkSeparator: String = defaultSeparator): String
  def mkMenuResponsePaginated(menuTab: MenuTab,
                              page: Int,
                              itemsPerPage: Int): (String, InlineKeyboardMarkup)

  def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemDataProvider: ItemDataProvider)
    extends ResponseService
    with StrictLogging {

  @deprecated("Move to ServerResponseService")
  override def mkMenuResponse(itemLinkSeparator: String): String = {
    frag(
      itemDataProvider.allItems
        .map(i => mkItemInfo(i, inMenu = true))
        .toArray: _*
    ).render
  }

  override def mkMenuResponsePaginated(menuTab: MenuTab,
                                       page: Int,
                                       itemsPerPage: Int): (String, InlineKeyboardMarkup) = {

    // Filter everything without brewery since those are food.
    val items = itemDataProvider.allItems.filter(_.breweryInfo.name.isDefined).toList.sortBy(i => i.id)
    val renderedItems = frag(
      items
        .slice((page - 1) * itemsPerPage, (page - 1) * itemsPerPage + itemsPerPage)
        .map(i => mkItemInfo(i, inMenu = true))
        .toArray: _*
    ).render

    val markup = InlineKeyboardMarkup.singleRow(
      PaginationUtils
        .mkButtonsForPaginatedQuery(page, itemsPerPage, items.length)
        .map {
          case Button(bText, bCallbackData) =>
            InlineKeyboardButton.callbackData(bText, bCallbackData)
        }
    )

    (renderedItems, markup)
  }

  override def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup) = {

    val menuButton =
      InlineKeyboardMarkup.singleButton(InlineKeyboardButton.callbackData("Menu", "menu"))

    itemDataProvider.getItem(itemId) match {
      case Some(item) =>
        (mkItemInfo(item, inMenu = false).render, menuButton) // TODO: Separate response for a single item?
      case None => (mkItemNotFoundResponse(itemId), menuButton)
    }
  }

  private def mkItemNotFoundResponse(itemId: Int): String = {
    s"Позиции с ID '$itemId' не существует."
  }

  private def mkItemInfo(item: Item, inMenu: Boolean) = {
    frag(
      a(href := item.link.getOrElse("?"))("🍺 " + item.name.getOrElse("name = ?")),
      item.rating.map { case (v1, _) => i(s" $v1")}.getOrElse(" rating = ?").toString,
      "\n",
      s"Стиль: ${item.style.getOrElse("style = ?")}",
      "\n",
      s"Пивоварня: ${item.breweryInfo.name.getOrElse("breweryInfo.name = ?")}",
      "\n",
      item.draftType.getOrElse("draftType = ?") + " - " + item.price.map { case (c, price) => c + price }.getOrElse("?"),
      "\n",
      if (inMenu)
        s"Подробнее: /show${item.id}"
      else
        s"\n${item.description.getOrElse("?")}",
      "\n\n"
    )
  }
}
