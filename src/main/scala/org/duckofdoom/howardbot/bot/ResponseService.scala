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
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponse(
      page: Int
  )(implicit format: ResponseFormat): (String, InlineKeyboardMarkup) = {

    val beers = itemsProvider.beers.sortBy(i => i.id)

    mkPaginatedResponse(
      beers,
      page,
      p => CallbackUtils.mkMenuCallbackData(p.some, newMessage = false),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          mkBeerButtonInfo(beer)
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

    mkPaginatedResponse(
      stylesWithCounts.sortBy(_._2).reverse.map(_._1),
      page,
      p => CallbackUtils.mkStylesCallbackData(p.some, newMessage = false),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { style =>
      val count = stylesWithCountsMap.getOrElse(style, 0)
      format match {
        case ResponseFormat.TextMessage =>
          mkStyleHtmlInfo(style, count)
        case ResponseFormat.Buttons =>
          mkStyleButtonInfo(style, count)
      }

    }(callbackType = CallbackType.Styles)
  }

  override def mkBeersByStyleResponse(styleId: Int, page: Int)(
      implicit format: ResponseFormat
  ): (String, InlineKeyboardMarkup) = {
    val items = itemsProvider.findBeersByStyle(styleId)
    mkPaginatedResponse(
      items,
      page,
      p => CallbackUtils.mkItemsByStyleCallbackData(styleId, p),
      renderAsButtons = format == ResponseFormat.Buttons
    ) { beer =>
      format match {
        case ResponseFormat.TextMessage =>
          mkBeerHtmlInfo(beer, verbose = false, withStyleLink = false)
        case ResponseFormat.Buttons =>
          mkBeerButtonInfo(beer)
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
          mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render
        case ResponseFormat.Buttons =>
          mkBeerButtonInfo(beer)
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
          mkItemNotFoundResponse("Item", itemId),
          InlineKeyboardMarkup(Seq(CallbackUtils.mkAdditionalButtons(menu = true, styles = true)))
        )
    }
  }

  private def mkItemNotFoundResponse(
      itemType: String,
      itemId: Int
  ): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует."
  }

  // TODO: Move all string-producing stuff to separate class
  private def mkBeerHtmlInfo(
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

  private def mkBeerButtonInfo(beer: Beer): String = {
    // Multiline strings do not work anyways =(
    s"${beer.name.getOrElse("name = ?")}"
  }

  private def mkStyleHtmlInfo(style: Style, itemsCount: Int): String = {
    s"${style.name}: $itemsCount - ${Consts.showStylePrefix}${style.id}\n"
  }

  private def mkStyleButtonInfo(style: Style, itemsCount: Int): String = {
    s"${style.name}: $itemsCount"
  }

  private def mkPaginatedResponse[A <: Item, TPayload](
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
