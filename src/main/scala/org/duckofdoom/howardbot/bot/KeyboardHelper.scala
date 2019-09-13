package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.CallbackUtils.{
  mkChangeSortingCallback,
  mkMenuCallbackData,
  mkStylesCallbackData
}
import org.duckofdoom.howardbot.bot.Sorting.{
  Sorting,
  byBrewery,
  byBreweryDec,
  byName,
  byNameDec,
  byPriceForMl,
  byPriceForMlDec,
  byRating,
  byRatingDec,
  byStyle,
  byStyleDec
}
import org.duckofdoom.howardbot.utils.StaticData
import cats.syntax.option._

import scala.collection.mutable

class KeyboardHelper {

  def mkDefaultButtons(): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(Seq(mkAdditionalButtons(menu = true, styles = true, sorting = true)))
  }

  def mkPaginationButtons(
      paginationButtons: Seq[InlineKeyboardButton],
      menuButton: Boolean,
      stylesButton: Boolean,
      sortingButton: Boolean
  ): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(
      Seq(
        paginationButtons,
        mkAdditionalButtons(menuButton, stylesButton, sortingButton)
      )
    )
  }

  def mkChangeSortingButtons(currentSorting: Seq[Sorting]): InlineKeyboardMarkup = {
    var buttonsList = Seq(
      Seq(byName, byNameDec),
      Seq(byStyle, byStyleDec),
      Seq(byRating, byRatingDec),
      Seq(byPriceForMl, byPriceForMlDec),
      Seq(byBrewery, byBreweryDec)
    ).filter(s => {!currentSorting.exists(srt => srt.toString == s.head.toString)})
      .map(s => s.map(srt => InlineKeyboardButton.callbackData(srt.toHumanReadable, mkChangeSortingCallback(srt.some))))

    buttonsList :+= Seq(
      InlineKeyboardButton.callbackData(
        "Сбросить",
        mkChangeSortingCallback(Option.empty[Sorting])
      )
    )
    
    buttonsList ++= mkAdditionalButtons(menu = true, styles = true, sorting = false))

    InlineKeyboardMarkup(buttonsList)
  }

  private def mkAdditionalButtons(menu: Boolean, styles: Boolean, sorting: Boolean): Seq[InlineKeyboardButton] = {
    var buttonsList = mutable.MutableList[InlineKeyboardButton]()
    if (menu) {
      buttonsList += InlineKeyboardButton.callbackData(
        StaticData.menu,
        mkMenuCallbackData(None, newMessage = false)
      )
    }

    if (styles) {
      buttonsList += InlineKeyboardButton.callbackData(
        StaticData.styles,
        mkStylesCallbackData(None, newMessage = false)
      )
    }
    
    if (sorting) {
      buttonsList += InlineKeyboardButton.callbackData(
        StaticData.styles,
        mkChangeSortingCallback(None)
      )
    }

    buttonsList
  }
}
