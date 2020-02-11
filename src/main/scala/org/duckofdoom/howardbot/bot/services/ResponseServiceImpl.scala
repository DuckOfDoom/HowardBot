package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.InlineKeyboardMarkup
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.bot.utils.{Callback, Sorting}
import slogging.StrictLogging
import cats.syntax.option._
import scalatags.Text.all._

class ResponseServiceImpl(
    implicit itemsProvider: ItemsProvider,
    responseHelper: ResponseHelper,
    keyboardHelper: KeyboardHelper,
    config: Config
) extends ResponseService
    with StrictLogging {

  override def mkMenuResponse(
      page: Int,
      sortings: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {

    val beers = Sorting.sort(itemsProvider.availableBeers, sortings).toList

    responseHelper.mkPaginatedResponse(
      beers,
      page,
      Callback.Type.Menu,
      p => Callback.mkMenuCallbackData(p.some, newMessage = false)
    ) { beer =>
      responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }(ResponseFormat.TextMessage)
  }

  override def mkStylesResponse(
      page: Int
  ): (String, InlineKeyboardMarkup) = {

    val stylesWithCounts = itemsProvider.availableStyles
      .zip(itemsProvider.availableStyles.map { st =>
        itemsProvider.findBeerByStyleId(st.id).length
      })

    val stylesWithCountsMap = stylesWithCounts.toMap

    responseHelper.mkPaginatedResponse(
      stylesWithCounts.toList.sortBy(_._2).reverse.map(_._1),
      page,
      Callback.Type.Styles,
      p => Callback.mkStylesCallbackData(p.some, newMessage = false)
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      responseHelper.mkStyleButtonInfo(style, count)
    }(ResponseFormat.Buttons)
  }

  override def mkSettingsResponse(notificationsEnabled: Boolean): (String, InlineKeyboardMarkup) = {
    val message = "Настройки:"
    val buttons = keyboardHelper.mkSettingsButtons(notificationsEnabled)
    (message, buttons)
  }

  override def mkChangeSortingResponse(
      selectedSorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val message =
      s"""Текущая сортировка:\n${if (selectedSorting.isEmpty) "Нет"
      else selectedSorting.map(s => s"|${s.toHumanReadable}|").mkString("\n")}
      """

    val buttons = keyboardHelper.mkChangeSortingButtons(selectedSorting)
    (message, buttons)
  }

  override def mkToggleNotificationsResponse(notificationsEnabled: Boolean): (String, InlineKeyboardMarkup) = {
    val message = s"Уведомления ${if (notificationsEnabled) "включены" else "выключены"}"
    val buttons = keyboardHelper.mkSettingsButtons(notificationsEnabled)
    (message, buttons)
  }

  override def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val items = Sorting.sort(itemsProvider.findBeerByStyleId(styleId), sorting).toList
    responseHelper.mkPaginatedResponse(
      items,
      page,
      Callback.Type.ItemsByStyle,
      p => Callback.mkItemsByStyleCallbackData(styleId, p)
    ) { beer =>
      responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }
  }
  
  override def mkBeerResponse(beer: Beer): (String, InlineKeyboardMarkup) = {
    (
      responseHelper.mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render,
      keyboardHelper.mkDefaultButtons()
    )
  }

  override def mkBeerResponse(itemId: Int): (String, InlineKeyboardMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) => mkBeerResponse(beer)
      case None =>
        (
          responseHelper.mkItemNotFoundResponse("Item", itemId),
          keyboardHelper.mkDefaultButtons()
        )
    }
  }

  override def mkSearchBeerByNameResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val searchResults = Sorting
      .sort(itemsProvider.availableBeers, sorting)
      .toList
      .withFilter(b => b.name.isDefined)
      .withFilter(b => b.name.get.toLowerCase.contains(query.toLowerCase))
      .map(identity)

    if (searchResults.isEmpty)
      return (responseHelper.mkEmptySeachResultsResponse(query), keyboardHelper.mkDefaultButtons())

    responseHelper.mkPaginatedResponse(
      searchResults,
      page,
      Callback.Type.SearchBeerByName,
      p => Callback.mkSearchBeerByNameCallback(query, p)
    ) { beer =>
      responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }
  }

  override def mkSearchBeerByStyleResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val searchResults = Sorting
      .sort(itemsProvider.availableBeers, sorting)
      .toList
      .withFilter(b => b.style.isDefined)
      .withFilter(b => b.style.get.toLowerCase.contains(query.toLowerCase))
      .map(identity)

    if (searchResults.isEmpty)
      return (responseHelper.mkEmptySeachResultsResponse(query), keyboardHelper.mkDefaultButtons())

    responseHelper.mkPaginatedResponse(
      searchResults,
      page,
      Callback.Type.SearchBeerByStyle,
      p => Callback.mkSearchBeerByStyleCallback(query, p)
    ) { beer =>
      responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
    }
  }

  override def formatNotification(title: String, message: String): String = {
    frag(b(title), "\n", message).render
  }
}