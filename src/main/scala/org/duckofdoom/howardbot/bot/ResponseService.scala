package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.utils.Extensions._
import CallbackUtils.CallbackType
import CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{Beer, Item, Style, ItemsProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.PaginationUtils
import scalatags.generic
import scalatags.text.Builder
import slogging.StrictLogging
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.ResponseFormat.ResponseFormat

object ResponseFormat extends Enumeration {
  type ResponseFormat = Value
  val TextMessage, Buttons = Value
}

trait ResponseService {
  def mkMenuResponse(page: Int)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)
  def mkBeerResponse(beerId: Int)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)
  def mkBeerResponse(beer: Beer)(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)

  def mkBeersByStyleResponse(
      styleId: Int,
      page: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup)

//  def mkSearchForBeerResponse(query: String, page: Int)(
//      implicit format: ResponseFormat
//  ): (String, InlineKeyboardMarkup)
//
//  def mkSearchForStyleResponse(query: String, page: Int)(
//      implicit format: ResponseFormat
//  ): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {
  
  val helper = new ResponseHelper()

  override def mkMenuResponse(
      page: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {

    val beers = itemsProvider.beers.sortBy(i => i.id)

    helper.mkPaginatedResponse(
      beers,
      page,
      p => CallbackUtils.mkMenuCallbackData(p.some, newMessage = false),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          helper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          helper.mkBeerButtonInfo(beer)
      }
    }(CallbackType.Menu)
  }

  override def mkStylesResponse(
      page: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {

    val stylesWithCounts = itemsProvider.styles
      .zip(itemsProvider.styles.map { st =>
        itemsProvider.findBeersByStyle(st.id).length
      })

    val stylesWithCountsMap = stylesWithCounts.toMap

    helper.mkPaginatedResponse(
      stylesWithCounts.sortBy(_._2).reverse.map(_._1),
      page,
      p => CallbackUtils.mkStylesCallbackData(p.some, newMessage = false),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      format match {
        case ResponseFormat.TextMessage =>
          helper.mkStyleHtmlInfo(style, count)
        case ResponseFormat.Buttons =>
          helper.mkStyleButtonInfo(style, count)
      }

    }(callbackType = CallbackType.Styles)
  }

  override def mkBeersByStyleResponse(styleId: Int, page: Int)(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup) = {
    val items = itemsProvider.findBeersByStyle(styleId)
    helper.mkPaginatedResponse(
      items,
      page,
      p => CallbackUtils.mkItemsByStyleCallbackData(styleId, p),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          helper.mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          helper.mkBeerButtonInfo(beer)
      }
    }(CallbackType.ItemsByStyle)
  }

  override def mkBeerResponse(
      beer: Beer
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {
    // TODO: Separate response for a single beer?
    (
      format match {
        case ResponseFormat.TextMessage =>
          helper.mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render
        case ResponseFormat.Buttons =>
          helper.mkBeerButtonInfo(beer)
      },
      InlineKeyboardMarkup(Seq(CallbackUtils.mkAdditionalButtons(menu = true, styles = true)))
    )
  }

  override def mkBeerResponse(
      itemId: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) => mkBeerResponse(beer)
      case None =>
        (
          helper.mkItemNotFoundResponse("Item", itemId),
          InlineKeyboardMarkup(Seq(CallbackUtils.mkAdditionalButtons(menu = true, styles = true)))
        )
    }
  }
}
