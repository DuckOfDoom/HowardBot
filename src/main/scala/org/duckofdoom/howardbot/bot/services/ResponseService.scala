package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.InlineKeyboardMarkup
import org.duckofdoom.howardbot.bot.data.Beer
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting

object ResponseFormat extends Enumeration {
  type ResponseFormat = Value
  val TextMessage, Buttons = Value
}

/**
  * A service to create responses for user commands
  */
trait ResponseService {

  /**
    * Show whole menu
    */
  def mkMenuResponse(page: Int, sorting: Seq[Sorting]): (String, InlineKeyboardMarkup)

  /**
    * Show available styles
    */
  def mkStylesResponse(page: Int): (String, InlineKeyboardMarkup)

  /**
    * Show change settings dialogue
    */
  def mkSettingsResponse(notificationsEnabled: Boolean): (String, InlineKeyboardMarkup)

  /**
    * Show change sorting dialogue
    */
  def mkChangeSortingResponse(selectedSorting: Seq[Sorting]): (String, InlineKeyboardMarkup)

  /**
    * Show changed notifications state
    */
  def mkToggleNotificationsResponse(notificationsEnabled:Boolean): (String, InlineKeyboardMarkup)
  
  /**
    * Show a single beer by id
    */
  def mkBeerResponse(beerId: Int): (String, InlineKeyboardMarkup)

  /**
    * Show a single beer by id
    */
  def mkBeerResponse(beer: Beer): (String, InlineKeyboardMarkup)

  /**
    * Show beers by style id
    */
  def mkBeersByStyleResponse(
      styleId: Int,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)

  /**
    * Search beers by name
    */
  def mkSearchBeerByNameResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)

  /**
    * Search beers by style
    */
  def mkSearchBeerByStyleResponse(
      query: String,
      page: Int,
      sorting: Seq[Sorting]
  ): (String, InlineKeyboardMarkup)

  // TODO: Remove?
  def formatNotification(title: String, message: String): String
}
