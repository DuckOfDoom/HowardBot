package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.CallbackUtils.{mkMenuCallbackData, mkStylesCallbackData}
import org.duckofdoom.howardbot.utils.StaticData

import scala.collection.mutable

class KeyboardHelper {

  def mkDefaultButtons(): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(Seq(mkAdditionalButtons(menu = true, styles = true)))
  }

  def mkPaginationButtons(
      paginationButtons: Seq[InlineKeyboardButton],
      menuButton: Boolean,
      stylesButton: Boolean
  ): InlineKeyboardMarkup = {
    InlineKeyboardMarkup(
      Seq(
        paginationButtons,
        mkAdditionalButtons(menuButton, stylesButton)
      )
    )
  }

  private def mkAdditionalButtons(menu: Boolean, styles: Boolean): Seq[InlineKeyboardButton] = {
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

    buttonsList
  }
}
