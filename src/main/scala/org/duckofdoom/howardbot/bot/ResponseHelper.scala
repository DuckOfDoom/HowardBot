package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemsProvider, Style}
import org.duckofdoom.howardbot.utils.PaginationUtils
import org.duckofdoom.howardbot.utils.Extensions._
import scalatags.Text.all._
import scalatags.generic
import scalatags.text.Builder

class ResponseHelper(implicit config: Config, itemsProvider: ItemsProvider) {

  def mkItemNotFoundResponse(
      itemType: String,
      itemId: Int
  ): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует."
  }

  // TODO: Move all string-producing stuff to separate class
  def mkBeerHtmlInfo(
      beer: Beer,
      verbose: Boolean,
      withStyleLink: Boolean
  ): generic.Frag[Builder, String] = {
    frag(
      a(href := beer.link.getOrElse("?"))("🍺 " + beer.name.getOrElse("name = ?")),
      beer.rating.map { case (v1, _) => s" $v1" }.getOrElse(" rating = ?").toString,
      "\n",
      s"Стиль: ${beer.style
      // TODO: Think about error handling here
        .map(style => {
          if (withStyleLink)
            s"$style (${Consts.showStylePrefix}${itemsProvider.getStyleId(style).getOrElse("BROKEN")})"
          else
            style.toString
        })
        .getOrElse("style = ? ")}",
      "\n",
      s"Пивоварня: ${beer.breweryInfo.name.getOrElse("breweryInfo.name = ?")}",
      "\n",
      beer.draftType.getOrElse("draftType = ?") + " - " + beer.price
        .map { case (c, price) => c + price }
        .getOrElse("?"),
      "\n",
      if (!verbose)
        s"Подробнее: ${Consts.showItemPrefix}${beer.id}"
      else
        s"\n${beer.description.getOrElse("?")}",
      "\n\n"
    )
  }

  def mkBeerButtonInfo(beer: Beer): String = {
    // Multiline strings do not work anyways =(
    s"${beer.name.getOrElse("name = ?")}"
  }

  def mkStyleHtmlInfo(style: Style, itemsCount: Int): String = {
    s"${style.name}: $itemsCount - ${Consts.showStylePrefix}${style.id}\n"
  }

  def mkStyleButtonInfo(style: Style, itemsCount: Int): String = {
    s"${style.name}: $itemsCount"
  }

  def mkPaginatedResponse[A <: Item, TPayload](
      allItems: List[A],
      page: Int,
      mkCallbackData: Int => String,
      // Try to render allItems as buttons, do not render them into message
      renderAsButtons: Boolean
  )(
      renderItem: A => generic.Frag[Builder, String]
  )(implicit callbackType: CallbackType): (String, InlineKeyboardMarkup) = {

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
          .mkButtonsForPaginatedQuery(p, itemsPerPage, allItems.length, mkCallbackData),
        CallbackUtils.mkAdditionalButtons(
          callbackType != CallbackType.Menu,
          callbackType != CallbackType.Styles
        )
      )
    )

    val selectedItems =
      allItems.slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)

    // Instead of rendering allItems into a message, render them as buttons
    var messageContents: String      = "Пожалуйста:"
    var markup: InlineKeyboardMarkup = paginationMarkup

    if (renderAsButtons) {
      val itemsMarkup = InlineKeyboardMarkup.singleColumn(
        selectedItems.map(item => {
          InlineKeyboardButton(
            renderItem(item).render,
            CallbackUtils.mkItemCallback(item)
          )
        })
      )

      // Merge markup with pagination.
      // Layout is as follows: Seq( Row(Column(..), Column(..), ..), Row(Column(..), Column(..), ..), ..)

      markup = InlineKeyboardMarkup(itemsMarkup.inlineKeyboard ++ markup.inlineKeyboard)

    } else {
      messageContents = frag(
        selectedItems.map(i => {
          renderItem(i)
        })
      ).render
    }

    (messageContents, markup)
  }
}
