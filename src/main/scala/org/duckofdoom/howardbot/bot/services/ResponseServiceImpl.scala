package org.duckofdoom.howardbot.bot.services

import org.duckofdoom.howardbot.utils.Extensions._
import cats.syntax.option._
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup, ReplyMarkup}
import org.duckofdoom.howardbot.bot.Consts
import org.duckofdoom.howardbot.bot.data.{Beer, Item, Style}
import org.duckofdoom.howardbot.bot.utils.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.bot.utils.{Callback, ResponseFormat, Sorting}
import org.duckofdoom.howardbot.utils.PaginationUtils
import scalatags.Text.all.{a, frag, href, _}
import scalatags.text.Builder
import slogging.StrictLogging

class ResponseServiceImpl(
    stylesPerPage: Int,
    menuItemsPerPage: Int,
    itemsProvider: ItemsProvider,
    keyboardHelper: KeyboardService
) extends ResponseService
    with StrictLogging {

  type HtmlFragment = scalatags.generic.Frag[Builder, String]

  logger.info(s"Created: stylesPerPage:$stylesPerPage, menuItemsPerPage:$menuItemsPerPage")

  override def mkMenuResponse(
      page: Int,
      sortings: Seq[Sorting]
  ): (String, ReplyMarkup) = {

    val beers = Sorting.sort(itemsProvider.availableBeers, sortings).toList

    mkPaginatedResponse(
      beers,
      page,
      Callback.Type.Menu,
      p => Callback.mkMenuCallbackData(p.some)
    ) { beer =>
      mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }(ResponseFormat.TextMessage)
  }

  override def mkStylesResponse(
      page: Int
  ): (String, ReplyMarkup) = {

    val availableStyles = itemsProvider.getAvailableStyles(true)
    val stylesWithCounts = availableStyles.zip(
      availableStyles.map { st =>
        itemsProvider.findBeerByStyleId(st.id).length
      }
    )

    val stylesWithCountsMap = stylesWithCounts.toMap
    mkPaginatedResponse(
      stylesWithCounts.toList.sortBy(_._2).reverse.map(_._1),
      page,
      Callback.Type.Styles,
      p => Callback.mkStylesCallbackData(p.some)
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      mkStyleButtonInfo(style, count)
    }(ResponseFormat.Buttons)
  }

  override def mkSettingsResponse(notificationsEnabled: Boolean): (String, ReplyMarkup) = {
    val message = "Настройки:"
    val buttons = keyboardHelper.mkSettingsButtons(notificationsEnabled)
    (message, buttons)
  }

  override def mkChangeSortingResponse(
      selectedSorting: Seq[Sorting]
  ): (String, ReplyMarkup) = {
    val message =
      s"""Текущая сортировка:\n${if (selectedSorting.isEmpty) "Нет"
      else selectedSorting.map(s => s"|${s.toHumanReadable}|").mkString("\n")}
      """

    val buttons = keyboardHelper.mkChangeSortingButtons(selectedSorting)
    (message, buttons)
  }

  override def mkToggleNotificationsResponse(notificationsEnabled: Boolean): (String, ReplyMarkup) = {
    val message = s"Уведомления ${if (notificationsEnabled) "включены" else "выключены"}"
    val buttons = keyboardHelper.mkSettingsButtons(notificationsEnabled)
    (message, buttons)
  }

  override def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, ReplyMarkup) = {
    val items = Sorting.sort(itemsProvider.findBeerByStyleId(styleId), sorting).toList
    mkPaginatedResponse(
      items,
      page,
      Callback.Type.ItemsByStyle,
      p => Callback.mkItemsByStyleCallbackData(styleId, p.some)
    ) { beer =>
      mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }
  }

  override def mkBeerResponse(beer: Beer): (String, ReplyMarkup) = {
    (
      mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render,
      keyboardHelper.mkDefaultButtons()
    )
  }

  override def mkBeerResponse(itemId: Int): (String, ReplyMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) => mkBeerResponse(beer)
      case None =>
        (
          mkBeerNotFoundResponse(itemId),
          keyboardHelper.mkDefaultButtons()
        )
    }
  }

  override def mkSearchResponse(
      query: String,
      searchResults: Seq[Beer],
      page: Int
  ): (String, ReplyMarkup) = {

    mkPaginatedResponse(
      searchResults,
      page,
      Callback.Type.Search,
      p => Callback.mkSearchCallback(query, p.some)
    ) { beer =>
      mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }
  }

  override def mkEmptySearchResultsResponse(
      query: String
  ): (String, InlineKeyboardMarkup) = {
    (s"По запросу '$query' ничего не найдено :(", keyboardHelper.mkDefaultButtons())
  }

  override def formatNotification(title: String, message: String): String = {
    frag(b(title), "\n", message).render
  }

  private def mkBeerNotFoundResponse(
      itemId: Int
  ): String = {
    s"Пива с ID '$itemId' не существует :("
  }

  private def mkBeerHtmlInfo(
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

  private def mkStyleButtonInfo(style: Style, itemsCount: Int): HtmlFragment = {
    s"${style.name}: $itemsCount"
  }

  private def mkPaginatedResponse[A <: Item, TPayload](
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
  ): (String, ReplyMarkup) = {

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
