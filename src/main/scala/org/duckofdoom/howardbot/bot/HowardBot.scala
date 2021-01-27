package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.ReplyMarkup
import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.db.dto.User

/**
  * This class processes requests from users
  */
trait HowardBot {

  type Response = (String, ReplyMarkup)

  /**
    * Show whole menu as a paginated response
    */
  def showMenu(page: Option[Int] = None)(implicit user: User): Response

  /**
    * Show styles list
    */
  def showStyles(page: Option[Int] = None)(implicit user: User): Response

  /**
    * Show beers of specific style
    */
  def showBeersByStyle(style: Int, page: Option[Int] = None)(implicit user: User): Response

  /**
    * Show specific item (either beers by one style, or specific beer)
    */
  def showItem(itemType: ItemType, itemId: Int)(implicit user: User): Response

  /**
    * Search everything in order (first by beer name, then by brewery, then by style)
    */
  def search(query: String, page: Option[Int])(implicit user: User): Response

  /**
    * Show bot settings
    */
  def showSettings()(implicit user: User): Response

  /**
    * Change sorting in settings
    */
  def changeSorting(mSorting: Either[Unit, Option[Sorting]])(implicit user: User): Response

  /**
    * Toggle bot notifications
    */
  def toggleNotifications()(implicit user: User): (String, ReplyMarkup)
}
