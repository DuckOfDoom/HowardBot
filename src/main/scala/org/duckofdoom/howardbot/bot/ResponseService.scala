package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.utils.Extensions._
import CallbackUtils.CallbackType
import CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{Item, ItemsProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import scalatags.generic
import scalatags.text.Builder
import slogging.StrictLogging
import cats.syntax.option._

trait ResponseService {
  def mkMenuResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup)
  def mkItemsByStyleResponse(styleId: Int, page: Int): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponse(page: Int): (String, InlineKeyboardMarkup) = {
    // Filter everything without brewery since those are food.
    val items =
      itemsProvider.items.filter(_.breweryInfo.name.isDefined).sortBy(i => i.id)

    mkPaginatedResponse(items, page, None) { item =>
      mkItemInfo(item, inMenu = true)
    }(CallbackType.Menu)
  }

  override def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup) = {
    mkPaginatedResponse(itemsProvider.styles.sorted, page, None) { style =>
      val itemsByStyleCount = itemsProvider.findItemsByStyle(style).length
      frag(s"""$style: $itemsByStyleCount - ${Consts.showStylePrefix}${itemsProvider.getStyleId(style).getOrElse("?")}\n""")
    }(callbackType = CallbackType.Styles)
  }

  // Concrete style can be rendered with pagination
  override def mkItemsByStyleResponse(styleId: Int, page: Int): (String, InlineKeyboardMarkup) = {
    val items = itemsProvider.findItemsByStyle(styleId)
    mkPaginatedResponse(items, page, styleId.some) { i =>
      mkItemInfo(i, inMenu = true)
    }(CallbackType.ItemsByStyle)
  }

  override def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup) = {
    itemsProvider.getItem(itemId) match {
      case Some(item) =>
        // TODO: Separate response for a single item?
        (mkItemInfo(item, inMenu = false).render,
         InlineKeyboardMarkup(Seq(PaginationUtils.mkAdditionalButtons(menu = true, styles = true))))
      case None =>
        (mkItemNotFoundResponse("Item", itemId),
         InlineKeyboardMarkup(Seq(PaginationUtils.mkAdditionalButtons(menu = true, styles = true))))
    }
  }

  private def mkItemNotFoundResponse(itemType: String, itemId: Int): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует."
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
        s"Подробнее: ${Consts.showItemPrefix}${item.id}"
      else
        s"\n${item.description.getOrElse("?")}",
      "\n\n"
    )
  }

  private def mkPaginatedResponse[A, TPayload](
      items: List[A],
      page: Int,
      payload: Option[TPayload])(renderItem: A => generic.Frag[Builder, String])(
      implicit callbackType: CallbackType): (String, InlineKeyboardMarkup) = {

    val itemsPerPage = callbackType match {
      case CallbackType.Styles                           => config.stylesPerPage
      case CallbackType.Menu | CallbackType.ItemsByStyle => config.menuItemsPerPage
      case _                                             => throw new Exception(s"Unknown callback type '$callbackType'!")
    }

    var totalPages = items.length / itemsPerPage
    if (items.length % itemsPerPage != 0)
      totalPages += 1
    
    val p = page.clamp(1, totalPages)
    val renderedItems = frag(
      items
        .slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)
        .map(renderItem)
        .toArray: _*
    ).render

    val markup = InlineKeyboardMarkup(
      Seq(
        PaginationUtils
          .mkButtonsForPaginatedQuery(p, itemsPerPage, items.length, payload)
          .map {
            case Button(bText, bCallbackData) =>
              InlineKeyboardButton.callbackData(bText, bCallbackData)
          },
        PaginationUtils.mkAdditionalButtons(callbackType != CallbackType.Menu,
                                            callbackType != CallbackType.Styles)
      )
    )

    (renderedItems, markup)
  }
}
