package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Static.CallbackType
import org.duckofdoom.howardbot.bot.data.{Item, ItemsProvider, Static}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import slogging.StrictLogging

trait ResponseService {
  def mkMenuResponsePaginated(page: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup)
  def mkStylesResponse(): String
  def mkItemsByStyleResponse(page: Int, query:String): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponsePaginated(page: Int): (String, InlineKeyboardMarkup) = {

    implicit val callbackType: Static.CallbackType.Value = CallbackType.ItemsByStyle
    
    val itemsPerPage = config.menuItemsPerPage
    val p = if (page < 1) 1 else page

    // Filter everything without brewery since those are food.
    val items =
      itemsProvider.items.filter(_.breweryInfo.name.isDefined).sortBy(i => i.id)
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

    itemsProvider.getItem(itemId) match {
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

  override def mkStylesResponse(): String = {
    frag (
      itemsProvider.styles
    ).render
  }

  override def mkItemsByStyleResponse(page: Int, query:String): (String, InlineKeyboardMarkup) = {
    // TODO: Make generic method to generate paginated requests
    
    implicit val callbackType: Static.CallbackType.Value = CallbackType.ItemsByStyle
    
    val itemsPerPage = config.menuItemsPerPage
    val items = itemsProvider.findItemsByStyle(query)
    val p = if (page < 1) 1 else page
    
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
}
