package org.duckofdoom.howardbot.bot.services

import cats.syntax.option._
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.bot.utils.Sorting._

import scala.collection.mutable

trait KeyboardHelper {
  def mkDefaultButtons(settings: Boolean = true): InlineKeyboardMarkup

  def mkPaginationButtons(
      paginationButtons: Seq[InlineKeyboardButton],
      menuButton: Boolean,
      stylesButton: Boolean
  ): InlineKeyboardMarkup

  def mkSettingsButtons(notificationsEnabled: Boolean): InlineKeyboardMarkup
  def mkChangeSortingButtons(currentSorting: Seq[Sorting]): InlineKeyboardMarkup
}

class KeyboardHelperImpl extends KeyboardHelper {

  def mkDefaultButtons(settings: Boolean = true): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(Seq(mkAdditionalButtons(menu = true, styles = true, settings = settings)))
  }

  def mkPaginationButtons(
      paginationButtons: Seq[InlineKeyboardButton],
      menuButton: Boolean,
      stylesButton: Boolean
  ): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(
      Seq(
        paginationButtons,
        mkAdditionalButtons(menuButton, stylesButton, settings = true)
      )
    )
  }

  def mkSettingsButtons(notificationsEnabled: Boolean): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(
      Seq(
        Seq(
          InlineKeyboardButton.callbackData("Изменить сортировку", Callback.mkChangeSortingCallback(Left(Unit))),
          InlineKeyboardButton.callbackData(
            s"${if (notificationsEnabled) "Выключить" else "Включить"} уведомления",
            Callback.mkToggleNotificationsCallback()
          )
        ),
        mkAdditionalButtons(menu = true, styles = true, settings = false)
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

    buttonsList :+= mkAdditionalButtons(menu = true, styles = true, settings = true)

    InlineKeyboardMarkup(buttonsList)
  }

  private def mkAdditionalButtons(
      menu: Boolean,
      styles: Boolean,
      settings: Boolean
  ): Seq[InlineKeyboardButton] = {
    var buttonsList = mutable.ListBuffer[InlineKeyboardButton]()
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

    if (settings) {
      buttonsList += InlineKeyboardButton.callbackData(
        "Настройки",
        Callback.mkSettingsCallback()
      )
    }

    buttonsList
  }
}
