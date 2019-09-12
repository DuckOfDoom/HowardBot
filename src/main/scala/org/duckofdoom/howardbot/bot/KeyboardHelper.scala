package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.CallbackUtils.{mkMenuCallbackData, mkStylesCallbackData,mkChangeSortingCallback}
import org.duckofdoom.howardbot.bot.Sorting.Sorting
import org.duckofdoom.howardbot.utils.StaticData
import cats.syntax.option._

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
  
  def mkChangeSortingButtons(currentSorting: Seq[Sorting]): InlineKeyboardMarkup = {
    var buttonsList = mutable.MutableList[InlineKeyboardButton]()
    
    for (s <- Sorting.all){
      if (!currentSorting.contains(s)){
        buttonsList += InlineKeyboardButton.callbackData(
          s.toHumanReadable,
          mkChangeSortingCallback(s.some)
        )
      }
    }
    
    buttonsList += InlineKeyboardButton.callbackData(
      "Закончить",
      mkChangeSortingCallback(Option.empty[Sorting])
    )
    
    InlineKeyboardMarkup(buttonsList.map(b => Seq(b)))
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
