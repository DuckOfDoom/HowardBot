package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.utils.Extensions._
import CallbackUtils.CallbackType
import CallbackUtils.CallbackType.CallbackType
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemsProvider}
import scalatags.Text.all._
import org.duckofdoom.howardbot.utils.PaginationUtils
import scalatags.generic
import scalatags.text.Builder
import slogging.StrictLogging
import cats.syntax.option._

trait ResponseService {
  def mkMenuResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup)
  def mkBeerResponse(itemId: Int): (String, InlineKeyboardMarkup)
  def mkBeersByStyleResponse(styleId: Int, page: Int): (String, InlineKeyboardMarkup)
}

class ResponseServiceImpl(implicit itemsProvider: ItemsProvider, config: Config)
    extends ResponseService
    with StrictLogging {

  override def mkMenuResponse(page: Int): (String, InlineKeyboardMarkup) = {
    // Filter everything without brewery since those are food.
    val items =
      itemsProvider.items.filter(_.breweryInfo.name.isDefined).sortBy(i => i.id)

    mkPaginatedResponse(items, page, p => CallbackUtils.mkMenuCallbackData(p.some, newMessage = true), renderAsButtons = true) { item =>
      mkBeerButtonInfo(item)
    }(CallbackType.Menu)
  }

  override def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup) = {
    mkPaginatedResponse(itemsProvider.styles.sorted, page, p => CallbackUtils.mkStylesCallbackData(p.some, newMessage = true), renderAsButtons = true) { style =>
      val itemsByStyleCount = itemsProvider.findBeersByStyle(style.name).length
      frag(
        s"""$style: $itemsByStyleCount - ${Consts.showStylePrefix}${
          itemsProvider
            .getStyleId(style.name)
            .getOrElse("?")
        }\n""")
    }(callbackType = CallbackType.Styles)
  }

  // Concrete style can be rendered with pagination
  override def mkBeersByStyleResponse(styleId: Int, page: Int): (String, InlineKeyboardMarkup) = {
    val items = itemsProvider.findBeersByStyle(styleId)
    mkPaginatedResponse(items, page, p => CallbackUtils.mkItemsByStyleCallbackData(styleId, p), renderAsButtons = true) { i =>
      mkBeerButtonInfo(i)
    }(CallbackType.ItemsByStyle)
  }

  override def mkBeerResponse(itemId: Int): (String, InlineKeyboardMarkup) = {
    itemsProvider.getBeer(itemId) match {
      case Some(beer) =>
        // TODO: Separate response for a single beer?
        (mkBeerHtmlInfo(beer, verbose = true, withStyleLink = true).render,
         InlineKeyboardMarkup(Seq(CallbackUtils.mkAdditionalButtons(menu = true, styles = true))))
      case None =>
        (mkItemNotFoundResponse("Item", itemId),
         InlineKeyboardMarkup(Seq(CallbackUtils.mkAdditionalButtons(menu = true, styles = true))))
    }
  }

  private def mkItemNotFoundResponse(itemType: String, itemId: Int): String = {
    s"Позиции с ID '$itemId' и типом '$itemType' не существует."
  }

  private def mkBeerHtmlInfo(beer: Beer,
                             verbose: Boolean,
                             withStyleLink: Boolean): generic.Frag[Builder, String] = {
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
    s"""${beer.name}
       |Стиль: ${beer.style}
       |Пивоварня: ${beer.breweryInfo.name.getOrElse("breweryInfo.name = ?")}",
       |${beer.draftType.getOrElse("draftType = ?") + " - " + beer.price
         .map { case (c, price) => c + price }
         .getOrElse("?")},
      "\n",
     """.stripMargin
  }

  private def mkPaginatedResponse[A <: Item, TPayload](
      allItems: List[A],
      page: Int,
      mkCallbackData: Int => String,
      // Try to render allItems as buttons, do not render them into message
      renderAsButtons: Boolean)(renderItem: A => generic.Frag[Builder, String])(
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
          .mkButtonsForPaginatedQuery(p, itemsPerPage, allItems.length, mkCallbackData),
        CallbackUtils.mkAdditionalButtons(callbackType != CallbackType.Menu,
                                            callbackType != CallbackType.Styles)
      )
    )

    val selectedItems =
      allItems.slice((p - 1) * itemsPerPage, (p - 1) * itemsPerPage + itemsPerPage)

    // Instead of rendering allItems into a message, render them as buttons
    var messageContents: String      = "Пожалуйста:"
    var markup: InlineKeyboardMarkup = paginationMarkup

    if (renderAsButtons) {
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
