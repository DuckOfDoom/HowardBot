package org.duckofdoom.howardbot.bot.services

import cats.syntax.option._
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.bot.utils.Sorting._

import scala.collection.mutable

class KeyboardHelper {

  def mkDefaultButtons(sorting: Boolean = true): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(Seq(mkAdditionalButtons(menu = true, styles = true, sorting = sorting)))
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
    ).filter(s => { !currentSorting.exists(srt => srt.toString == s.head.toString) })
      .map(
        s =>
          s.map(
            srt =>
              InlineKeyboardButton
                .callbackData(
                  srt.toHumanReadable,
                  Callback.mkChangeSortingCallback(Right(srt.some))
                )
          )
      )

    buttonsList :+= Seq(
      InlineKeyboardButton.callbackData(
        "Сбросить",
        Callback.mkChangeSortingCallback(Right(Option.empty[Sorting]))
      )
    )

    buttonsList :+= mkAdditionalButtons(menu = true, styles = true, sorting = false)

    InlineKeyboardMarkup(buttonsList)
  }

  private def mkAdditionalButtons(
      menu: Boolean,
      styles: Boolean,
      sorting: Boolean
  ): Seq[InlineKeyboardButton] = {
    var buttonsList = mutable.MutableList[InlineKeyboardButton]()
    if (menu) {
      buttonsList += InlineKeyboardButton.callbackData(
        "Меню",
        Callback.mkMenuCallbackData(None, newMessage = false)
      )
    }

    if (styles) {
      buttonsList += InlineKeyboardButton.callbackData(
        "Стили",
        Callback.mkStylesCallbackData(None, newMessage = false)
      )
    }

    if (sorting) {
      buttonsList += InlineKeyboardButton.callbackData(
        "Сортировка",
        Callback.mkChangeSortingCallback(Left(Unit))
      )
    }

    buttonsList
  }
}