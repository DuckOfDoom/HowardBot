package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import CallbackUtils.CallbackType
import CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{Item, ItemsProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import scalatags.generic
import scalatags.text.Builder
import slogging.StrictLogging

trait ResponseService {
  // TODO: Rename to mkMenuResponse
  def mkMenuResponsePaginated(page: Int): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup)
  def mkItemsByStyleResponse(page: Int, query: String): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponsePaginated(page: Int): (String, InlineKeyboardMarkup) = {
    // Filter everything without brewery since those are food.
    val items =
      itemsProvider.items.filter(_.breweryInfo.name.isDefined).sortBy(i => i.id)

    mkPaginatedResponse(items, page) { item =>
      mkItemInfo(item, inMenu = true)
    }(CallbackType.Menu)
  }

  override def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup) = {
    mkPaginatedResponse(itemsProvider.styles, page)(_.toString + "\n")(callbackType = CallbackType.Styles)
  }

  override def mkItemsByStyleResponse(page: Int, query: String): (String, InlineKeyboardMarkup) = {
    mkPaginatedResponse(itemsProvider.findItemsByStyle(query), page)(item => frag(item.toString + "\n"))(
      callbackType = CallbackType.ItemsByStyle)
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

  private def mkItemInfo(item: Item, inMenu: Boolean): generic.Frag[Builder, String] = {
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

  // TODO: Move to PaginationUtils
  private def mkPaginatedResponse[A](items: List[A], page: Int)(
      renderItem: A => generic.Frag[Builder, String])(
      implicit callbackType: CallbackType): (String, InlineKeyboardMarkup) = {

    val itemsPerPage = config.menuItemsPerPage
    val p            = if (page < 1) 1 else page

    val renderedItems = frag(
      items
        .slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)
        .map(renderItem)
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
