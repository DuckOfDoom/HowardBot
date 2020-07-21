package org.duckofdoom.howardbot.bot.services

import com.bot4s.telegram.models.ReplyMarkup
import org.duckofdoom.howardbot.bot.data.{Beer, Style}
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting

/**
  * A service to create responses for user commands
  */
trait ResponseService {

  /**
    * Show whole menu
    */
  def mkMenuResponse(beers: Seq[Beer], page: Int): (String, ReplyMarkup)

  /**
    * Show available styles
    */
  def mkStylesResponse(stylesWithCounts: Map[Style, Int], page: Int): (String, ReplyMarkup)

  /**
    * Show beers by style id
    */
  def mkBeersByStyleResponse(styleId: Int, beers: Seq[Beer], page: Int): (String, ReplyMarkup)

  /**
    * Make response for search query
    */
  def mkSearchResponse(query: String, searchResults: Seq[Beer], page: Int): (String, ReplyMarkup)

  /**
    * Show change settings dialogue
    */
  def mkSettingsResponse(notificationsEnabled: Boolean): (String, ReplyMarkup)

  /**
    * Show change sorting dialogue
    */
  def mkChangeSortingResponse(selectedSorting: Seq[Sorting]): (String, ReplyMarkup)

  /**
    * Show changed notifications state
    */
  def mkToggleNotificationsResponse(notificationsEnabled: Boolean): (String, ReplyMarkup)

  /**
    * Show a single beer by id
    */
  def mkBeerResponse(beerId: Int): (String, ReplyMarkup)

  /**
    * Show a single beer by id
    */
  def mkBeerResponse(beer: Beer): (String, ReplyMarkup)

  /**
    * Make response when nothing is found
    */
  def mkEmptySearchResultsResponse(
      query: String
  ): (String, ReplyMarkup)

  /**
    * Formats a notification for user
    */
  def formatNotification(title: String, message: String): String

}
