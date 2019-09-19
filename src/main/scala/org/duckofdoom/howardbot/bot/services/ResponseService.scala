package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.InlineKeyboardMarkup
import org.duckofdoom.howardbot.Config
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.bot.utils.{Callback, Sorting}
import slogging.StrictLogging

object ResponseFormat extends Enumeration {
  type ResponseFormat = Value
  val TextMessage, Buttons = Value
}

trait ResponseService {
  def mkMenuResponse(page: Int, sorting: Seq[Sorting]): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkChangeSortingResponse(selectedSorting: Seq[Sorting]): (String, InlineKeyboardMarkup)
  def mkBeerResponse(beerId: Int): (String, InlineKeyboardMarkup)
  def mkBeerResponse(beer: Beer): (String, InlineKeyboardMarkup)
  def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)
  def mkSearchBeerByNameResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)
  def mkSearchBeerByStyleResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  implicit val keyboardHelper: KeyboardHelper = new KeyboardHelper()
  val responseHelper                          = new ResponseHelper()

  override def mkMenuResponse(
      page: Int,
      sortings: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {

    val beers = Sorting.sort(itemsProvider.beers, sortings).toList

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

    val stylesWithCounts = itemsProvider.styles
      .zip(itemsProvider.styles.map { st =>
        itemsProvider.findBeersByStyle(st.id).length
      })

    val stylesWithCountsMap = stylesWithCounts.toMap

    responseHelper.mkPaginatedResponse(
      stylesWithCounts.sortBy(_._2).reverse.map(_._1),
      page,
      Callback.Type.Styles,
      p => Callback.mkStylesCallbackData(p.some, newMessage = false)
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      responseHelper.mkStyleButtonInfo(style, count)
    }(ResponseFormat.Buttons)
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

  override def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val items = Sorting.sort(itemsProvider.findBeersByStyle(styleId), sorting).toList
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
    // TODO: Separate response for a single beer?
    (
      responseHelper.mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render,
      keyboardHelper.mkDefaultButtons(sorting = false)
    )
  }

  override def mkBeerResponse(itemId: Int): (String, InlineKeyboardMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) => mkBeerResponse(beer)
      case None =>
        (
          responseHelper.mkItemNotFoundResponse("Item", itemId),
          keyboardHelper.mkDefaultButtons(sorting = false)
        )
    }
  }

  override def mkSearchBeerByNameResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup) = {
    val searchResults = Sorting
      .sort(itemsProvider.beers, sorting)
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
      .sort(itemsProvider.beers, sorting)
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
}
