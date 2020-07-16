package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import org.duckofdoom.howardbot.bot.utils.Sorting._

trait KeyboardService {
  def mkDefaultButtons(settings: Boolean = true): InlineKeyboardMarkup

  def mkPaginationButtons(
      paginationButtons: Seq[InlineKeyboardButton],
      menuButton: Boolean,
      stylesButton: Boolean
  ): InlineKeyboardMarkup

  def mkSettingsButtons(notificationsEnabled: Boolean): InlineKeyboardMarkup
  def mkChangeSortingButtons(currentSorting: Seq[Sorting]): InlineKeyboardMarkup
}


