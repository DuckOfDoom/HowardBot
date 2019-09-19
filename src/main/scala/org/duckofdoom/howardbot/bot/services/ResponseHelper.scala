package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemsProvider, Style}
import org.duckofdoom.howardbot.bot.services.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.Consts
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.utils.Extensions._
import org.duckofdoom.howardbot.utils.PaginationUtils
import scalatags.Text.all._
import scalatags.generic
import scalatags.text.Builder

class ResponseHelper(
    implicit config: Config,
    itemsProvider: ItemsProvider,
    keyboardHelper: KeyboardHelper
) {

  type HtmlFragment = generic.Frag[Builder, String]
  
  def mkItemNotFoundResponse(
      itemType: String,
      itemId: Int
  ): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует. :("
  }

  def mkEmptySeachResultsResponse(
      query: String
  ): String = {
    s"Ничего не найдено по запросу '$query'. :("
  }

  // TODO: Move all string-producing stuff to separate class
  def mkBeerHtmlInfo(
      beer: Beer,
      verbose: Boolean,
      withStyleLink: Boolean
  ): HtmlFragment = {
    frag(
      a(href := beer.link.getOrElse("link = ?"))("🍺 " + beer.name.getOrElse("name = ?")),
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
      beer.draftType match {
        case Some(dr) =>
          dr + " - " + beer.price
            .map { case (c, price) => c + price }
            .getOrElse("price = ?")
        case None => "On Deck"
      },
      "\n",
      if (!verbose)
        s"Подробнее: ${Consts.showItemPrefix}${beer.id}"
      else
        s"\n${beer.description.getOrElse("description = ?")}",
      "\n\n"
    )
  }

  def mkStyleButtonInfo(style: Style, itemsCount: Int): HtmlFragment = {
    s"${style.name}: $itemsCount"
  }

  def mkPaginatedResponse[A <: Item, TPayload](
      allItems: List[A],
      page: Int,
      // Type of callback data to attach to buttons
      callbackType: Callback.Type.Value,
      // A function to make callback data for each 'page' button.
      mkCallbackData: Int => String
  )(
      renderItem: A => HtmlFragment
  )(implicit responseFormat: ResponseFormat = ResponseFormat.TextMessage): (String, InlineKeyboardMarkup) = {

    val itemsPerPage = callbackType match {
      case Callback.Type.Styles => config.stylesPerPage
      case _                    => config.menuItemsPerPage
    }

    var totalPages = allItems.length / itemsPerPage
    if (allItems.length % itemsPerPage != 0)
      totalPages += 1

    val p = page.clamp(1, totalPages)
    val paginationMarkup =
      keyboardHelper.mkPaginationButtons(
        PaginationUtils
          .mkButtonsForPaginatedQuery(p, itemsPerPage, allItems.length, mkCallbackData),
        callbackType != Callback.Type.Menu,
        callbackType != Callback.Type.Styles,
        callbackType != Callback.Type.Styles
      )

    val selectedItems =
      allItems.slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)

    // Instead of rendering allItems into a message, render them as buttons
    var messageContents: String      = "Пожалуйста:"
    var markup: InlineKeyboardMarkup = paginationMarkup

    if (responseFormat == ResponseFormat.Buttons) {
      val itemsMarkup = InlineKeyboardMarkup.singleColumn(
        selectedItems.map(item => {
          InlineKeyboardButton(
            renderItem(item).render,
            Callback.mkItemCallback(item)
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
