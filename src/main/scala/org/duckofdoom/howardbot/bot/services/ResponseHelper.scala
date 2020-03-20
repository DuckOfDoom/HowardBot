package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemsProvider, Style}
import org.duckofdoom.howardbot.bot.services.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.Consts
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.utils.Extensions._
import org.duckofdoom.howardbot.utils.PaginationUtils
import scalatags.Text.all._
import scalatags.text.Builder
import slogging.StrictLogging

trait ResponseHelper {

  type HtmlFragment = scalatags.generic.Frag[Builder, String]

  def mkItemNotFoundResponse(
      itemType: String,
      itemId: Int
  ): String

  def mkEmptySeachResultsResponse(
      query: String
  ): String

  def mkBeerHtmlInfo(
      beer: Beer,
      verbose: Boolean,
      withStyleLink: Boolean
  ): HtmlFragment

  def mkStyleButtonInfo(style: Style, itemsCount: Int): HtmlFragment

  def mkPaginatedResponse[A <: Item, TPayload](
      allItems: Seq[A],
      page: Int,
      // Type of callback data to attach to buttons
      callbackType: Callback.Type.Value,
      // A function to make callback data for each 'page' button.
      mkCallbackData: Int => String
  )(
      renderItem: A => HtmlFragment
  )(
      implicit responseFormat: ResponseFormat = ResponseFormat.TextMessage
  ): (String, InlineKeyboardMarkup)
}

class ResponseHelperImpl(
    stylesPerPage: Int,
    menuItemsPerPage: Int,
    itemsProvider: ItemsProvider,
    keyboardHelper: KeyboardHelper
) extends ResponseHelper with StrictLogging {
  
  logger.info(s"Created: stylesPerPage:$stylesPerPage, menuItemsPerPage:$menuItemsPerPage")

  def mkItemNotFoundResponse(
      itemType: String,
      itemId: Int
  ): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует :("
  }

  def mkEmptySeachResultsResponse(
      query: String
  ): String = {
    s"По запросу '$query' ничего не найдено :("
  }

  def mkBeerHtmlInfo(
      beer: Beer,
      verbose: Boolean,
      withStyleLink: Boolean
  ): HtmlFragment = {
    frag(
      a(href := beer.link.getOrElse("link = ?"))("🍺 " + beer.name.getOrElse("name = ?")),
      " Рейтинг: " + beer.rating.map { case (v1, _) => s"$v1" }.getOrElse(" N/A").toString + "\n",
      s"Стиль: ${beer.style
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
      if (beer.isOnDeck)
        "On Deck"
      else
        beer.draftType.getOrElse("draftType = ?") + " - " + beer.price
          .map { case (c, price) => c + price }
          .getOrElse("price = ?"),
      "\n",
      beer.menuOrder match {
        case Some(tapNumber) => "Кран №" + tapNumber + "\n"
        case None            => ""
      },
      if (!verbose)
        s"Подробнее: ${Consts.showItemPrefix}${beer.id}"
      else
        s"\n${beer.description.getOrElse("")}",
      "\n\n"
    )
  }

  def mkStyleButtonInfo(style: Style, itemsCount: Int): HtmlFragment = {
    s"${style.name}: $itemsCount"
  }

  def mkPaginatedResponse[A <: Item, TPayload](
      allItems: Seq[A],
      page: Int,
      // Type of callback data to attach to buttons
      callbackType: Callback.Type.Value,
      // A function to make callback data for each 'page' button.
      mkCallbackData: Int => String
  )(
      renderItem: A => HtmlFragment
  )(
      implicit responseFormat: ResponseFormat = ResponseFormat.TextMessage
  ): (String, InlineKeyboardMarkup) = {

    if (allItems.isEmpty) {
      return ("Ничего не найдено :(", keyboardHelper.mkDefaultButtons())
    }

    val itemsPerPage = callbackType match {
      case Callback.Type.Styles => stylesPerPage
      case _                    => menuItemsPerPage
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
        callbackType != Callback.Type.Styles
      )

    val selectedItems =
      allItems.slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)

    var messageContents: String      = "Пожалуйста:"
    var markup: InlineKeyboardMarkup = paginationMarkup

    if (responseFormat == ResponseFormat.Buttons) {
      val itemsMarkup = InlineKeyboardMarkup.singleColumn(
        selectedItems.map(item => {
          InlineKeyboardButton(
            renderItem(item).render,
            Callback.mkSingleItemCallback(item)
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
