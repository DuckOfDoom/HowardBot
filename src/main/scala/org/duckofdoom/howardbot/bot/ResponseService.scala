package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.data.{Item, ItemsProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import slogging.StrictLogging

trait ResponseService {
  val defaultSeparator: String = ""
  def mkMenuResponsePaginated(page: Int, itemsPerPage: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemDataProvider: ItemsProvider)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponsePaginated(page: Int,
                                       itemsPerPage: Int): (String, InlineKeyboardMarkup) = {

    val p = if (page < 1) 1 else page

    // Filter everything without brewery since those are food.
    val items =
      itemDataProvider.items.filter(_.breweryInfo.name.isDefined).sortBy(i => i.id)
    val renderedItems = frag(
      items
        .slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)
        .map(i => mkItemInfo(i, inMenu = true))
        .toArray: _*
    ).render

    val markup = InlineKeyboardMarkup.singleRow(
      PaginationUtils
        .mkButtonsForPaginatedQuery(p, itemsPerPage, items.length)
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
      item.rating.map { case (v1, _) => s" $v1" }.getOrElse(" rating = ?").toString,
      "\n",
      s"Стиль: ${item.style.getOrElse("style = ?")}",
      "\n",
      s"Пивоварня: ${item.breweryInfo.name.getOrElse("breweryInfo.name = ?")}",
      "\n",
      item.draftType.getOrElse("draftType = ?") + " - " + item.price
        .map { case (c, price) => c + price }
        .getOrElse("?"),
      "\n",
      if (inMenu)
        s"Подробнее: /show${item.id}"
      else
        s"\n${item.description.getOrElse("?")}",
      "\n\n"
    )
  }
}
