package org.duckofdoom.howardbot.bot

import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardMarkup
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.Sorting.Sorting
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import scalatags.Text.all._
import slogging.StrictLogging

object ResponseFormat extends Enumeration {
  type ResponseFormat = Value
  val TextMessage, Buttons = Value
}

trait ResponseService {
  def mkMenuResponse(page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)
  def mkChangeSortingResponse(selectedSorting: Seq[Sorting]):(String, InlineKeyboardMarkup)
  def mkBeerResponse(beerId: Int)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)
  def mkBeerResponse(beer: Beer)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)

  def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)

  def mkSearchBeerByNameResponse(query: String, page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup)

  def mkSearchBeerByStyleResponse(query: String, page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
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
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {

    val beers = Sorting.sort(itemsProvider.beers, sortings).toList

    responseHelper.mkPaginatedResponse(
      beers,
      page,
      Callback.Type.Menu,
      p => CallbackUtils.mkMenuCallbackData(p.some, newMessage = false)
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          responseHelper.mkBeerButtonInfo(beer)
      }
    }
  }

  override def mkStylesResponse(
      page: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {

    val stylesWithCounts = itemsProvider.styles
      .zip(itemsProvider.styles.map { st =>
        itemsProvider.findBeersByStyle(st.id).length
      })

    val stylesWithCountsMap = stylesWithCounts.toMap

    responseHelper.mkPaginatedResponse(
      stylesWithCounts.sortBy(_._2).reverse.map(_._1),
      page,
      Callback.Type.Styles,
      p => CallbackUtils.mkStylesCallbackData(p.some, newMessage = false)
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkStyleHtmlInfo(style, count)
        case ResponseFormat.Buttons =>
          responseHelper.mkStyleButtonInfo(style, count)
      }
    }
  }

  override def mkChangeSortingResponse(selectedSorting: Seq[Sorting]): (String, InlineKeyboardMarkup) = {
    val message =
      s"""Текущая сортировка: ${
        if (selectedSorting.isEmpty) "Нет"
        else selectedSorting.map(_.toHumanReadable).mkString(", ")
      }
      """

    val buttons = keyboardHelper.mkChangeSortingButtons(selectedSorting)

    (message, buttons)
  }

  override def mkBeersByStyleResponse(styleId: Int, page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup) = {
    val items = Sorting.sort(itemsProvider.findBeersByStyle(styleId), sorting).toList
    responseHelper.mkPaginatedResponse(
      items,
      page,
      Callback.Type.ItemsByStyle,
      p => CallbackUtils.mkItemsByStyleCallbackData(styleId, p)
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          responseHelper.mkBeerButtonInfo(beer)
      }
    }
  }

  override def mkBeerResponse(
      beer: Beer
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {
    // TODO: Separate response for a single beer?
    (
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render
        case ResponseFormat.Buttons =>
          responseHelper.mkBeerButtonInfo(beer)
      },
      keyboardHelper.mkDefaultButtons()
    )
  }

  override def mkBeerResponse(
      itemId: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) => mkBeerResponse(beer)
      case None =>
        (
          responseHelper.mkItemNotFoundResponse("Item", itemId),
          keyboardHelper.mkDefaultButtons()
        )
    }
  }

  override def mkSearchBeerByNameResponse(query: String, page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup) = {
    val searchResults = Sorting.sort(itemsProvider.beers, sorting).toList
      .withFilter(b => b.name.isDefined)
      .withFilter(b => b.name.get.toLowerCase.contains(query.toLowerCase))
      .map(identity)

    if (searchResults.isEmpty)
      return (responseHelper.mkEmptySeachResultsResponse(query), keyboardHelper.mkDefaultButtons())

    responseHelper.mkPaginatedResponse(
      searchResults,
      page,
      Callback.Type.SearchBeerByName,
      p => CallbackUtils.mkSearchBeerByNameCallback(query, p)
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          responseHelper.mkBeerButtonInfo(beer)
      }
    }
  }

  override def mkSearchBeerByStyleResponse(query: String, page: Int, sorting: Seq[Sorting])(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup) = {
    val searchResults = Sorting.sort(itemsProvider.beers, sorting).toList
      .withFilter(b => b.style.isDefined)
      .withFilter(b => b.style.get.toLowerCase.contains(query.toLowerCase))
      .map(identity)

    if (searchResults.isEmpty)
      return (responseHelper.mkEmptySeachResultsResponse(query), keyboardHelper.mkDefaultButtons())

    responseHelper.mkPaginatedResponse(
      searchResults,
      page,
      Callback.Type.SearchBeerByStyle,
      p => CallbackUtils.mkSearchBeerByStyleCallback(query, p)
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          responseHelper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          responseHelper.mkBeerButtonInfo(beer)
      }
    }
  }
}
