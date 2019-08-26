package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.utils.Extensions._
import CallbackUtils.CallbackType
import CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{BaseItem, Item, ItemsProvider}
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

    mkPaginatedResponse(items, page, None, itemsAsButtons = true) { item =>
      mkItemInfo(item, inMenu = true, showStyle = true)
    }(CallbackType.Menu)
  }

  override def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup) = {
    mkPaginatedResponse(itemsProvider.styles.sorted, page, None,  itemsAsButtons = true) { style =>
      val itemsByStyleCount = itemsProvider.findItemsByStyle(style.name).length
      frag(s"""$style: $itemsByStyleCount - ${Consts.showStylePrefix}${itemsProvider
        .getStyleId(style.name)
        .getOrElse("?")}\n""")
    }(callbackType = CallbackType.Styles)
  }

  // Concrete style can be rendered with pagination
  override def mkItemsByStyleResponse(styleId: Int, page: Int): (String, InlineKeyboardMarkup) = {
    val items = itemsProvider.findItemsByStyle(styleId)
    mkPaginatedResponse(items, page, styleId.some, itemsAsButtons = true) { i =>
      mkItemInfo(i, inMenu = true, showStyle = false)
    }(CallbackType.ItemsByStyle)
  }

  override def mkItemResponse(itemId: Int): (String, InlineKeyboardMarkup) = {
    itemsProvider.getItem(itemId) match {
      case Some(item) =>
        // TODO: Separate response for a single item?
        (mkItemInfo(item, inMenu = false, showStyle = true).render,
         InlineKeyboardMarkup(Seq(PaginationUtils.mkAdditionalButtons(menu = true, styles = true))))
      case None =>
        (mkItemNotFoundResponse("Item", itemId),
         InlineKeyboardMarkup(Seq(PaginationUtils.mkAdditionalButtons(menu = true, styles = true))))
    }
  }

  private def mkItemNotFoundResponse(itemType: String, itemId: Int): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует."
  }

  private def mkItemInfo(item: Item,
                         inMenu: Boolean,
                         showStyle: Boolean): generic.Frag[Builder, String] = {
    frag(
      a(href := item.link.getOrElse("?"))("🍺 " + item.name.getOrElse("name = ?")),
      item.rating.map { case (v1, _) => s" $v1" }.getOrElse(" rating = ?").toString,
      "\n",
      s"Стиль: ${item.style
      // TODO: Think about error handling here
        .map(style => {
          if (showStyle)
            s"$style (${Consts.showStylePrefix}${itemsProvider.getStyleId(style).getOrElse("BROKEN")})"
          else
            style.toString
        })
        .getOrElse("style = ? ")}",
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

  private def mkPaginatedResponse[A <: BaseItem, TPayload](
      allItems: List[A],
      page: Int,
      paginationPayload: Option[TPayload],
      // Try to render allItems as buttons, do not render them into message
      itemsAsButtons:Boolean)(renderItem: A => generic.Frag[Builder, String])(
      implicit callbackType: CallbackType): (String, InlineKeyboardMarkup) = {

    val itemsPerPage = callbackType match {
      case CallbackType.Styles                           => config.stylesPerPage
      case CallbackType.Menu | CallbackType.ItemsByStyle => config.menuItemsPerPage
      case _                                             => throw new Exception(s"Unknown callback type '$callbackType'!")
    }

    var totalPages = allItems.length / itemsPerPage
    if (allItems.length % itemsPerPage != 0)
      totalPages += 1

    val p = page.clamp(1, totalPages)
    val paginationMarkup = InlineKeyboardMarkup(
      Seq(
        PaginationUtils
          .mkButtonsForPaginatedQuery(p, itemsPerPage, allItems.length, paginationPayload)
          .map {
            case Button(bText, bCallbackData) =>
              InlineKeyboardButton.callbackData(bText, bCallbackData)
          },
        PaginationUtils.mkAdditionalButtons(callbackType != CallbackType.Menu,
          callbackType != CallbackType.Styles)
      )
    )
    
    val selectedItems = allItems.slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)
    
    // Instead of rendering allItems into a message, render them as buttons
    var messageContents: String = "Пожалуйста:"
    var markup : InlineKeyboardMarkup = paginationMarkup
    
    if (itemsAsButtons) {
      val itemsMarkup = InlineKeyboardMarkup.singleColumn(
        selectedItems.map(i => {
          InlineKeyboardButton(renderItem(i).render, i.id.toString.some)
        })
      )
      
      // Merge markup with pagination. 
      // Layout is as follows: Seq( Row(Column(..), Column(..), ..), Row(Column(..), Column(..), ..), ..)
      
      markup = InlineKeyboardMarkup(itemsMarkup.inlineKeyboard ++ markup.inlineKeyboard)
      
    } else {
      messageContents = frag(
        selectedItems.map(i => {
          renderItem(i).render
        })
      ).render
    }

    (messageContents, markup)
  }
}
