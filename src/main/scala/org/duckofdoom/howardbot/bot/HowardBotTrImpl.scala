package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.ReplyMarkup
import org.duckofdoom.howardbot.bot.data.ItemType
import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.bot.services.ResponseService
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User

trait HowardBotTr {

  type Response = (String, ReplyMarkup)

  def showMenu(page: Option[Int] = None)(implicit user: User): Response

  def showStyles(page: Option[Int] = None)(implicit user: User): Response
  def showBeersByStyle(style: Int, page: Option[Int] = None)(implicit user: User): Response
  def showItem(itemType: ItemType, itemId: Int)(implicit user: User): Response
  def search(query: String, page: Option[Int])(implicit user: User): Response
  def showSettings()(implicit user: User): Response
  def changeSorting(mSorting: Either[Unit, Option[Sorting]])(implicit user: User): Response
  def toggleNotifications()(implicit user: User): (String, ReplyMarkup)
}

class HowardBotTrImpl(responseService: ResponseService, db: DB) extends HowardBotTr {

  override def showMenu(page: Option[Int] = None)(implicit user: User): Response = {
    val userState = page
      .map(p => db.updateUser(user.withMenuPage(p)))
      .getOrElse(user)
      .state

    responseService.mkMenuResponse(userState.menuPage, userState.sorting)
  }

  override def showStyles(page: Option[Int] = None)(implicit user: User): Response = {
    val stylesPage = page
      .map(p => db.updateUser(user.withMenuPage(p)))
      .getOrElse(user)
      .state
      .stylesPage

    responseService.mkStylesResponse(stylesPage)
  }

  override def showBeersByStyle(style: Int, page: Option[Int])(implicit user: User): Response = {
    responseService.mkBeersByStyleResponse(style, page.getOrElse(1), user.state.sorting)
  }

  override def showItem(itemType: ItemType, itemId: Int)(implicit user: User): Response = {
    itemType match {
      case ItemType.Beer  => responseService.mkBeerResponse(itemId)
      case ItemType.Style => responseService.mkBeersByStyleResponse(itemId, 1, user.state.sorting)
    }
  }

  override def search(query: String, page: Option[Int])(implicit user: User): (String, ReplyMarkup) = {
    responseService.mkSearchResponse(query, page.getOrElse(1), user.state.sorting)
  }

  override def showSettings()(implicit user: User): (String, ReplyMarkup) = {
    responseService.mkSettingsResponse(user.state.notificationsEnabled)
  }

  override def changeSorting(mSorting: Either[Unit, Option[Sorting]])(implicit user: User): (String, ReplyMarkup) = {

    if (mSorting.isRight) {
      // Reset menu page because we're going to change sorting and it wont make any sense.
      var newUser = user.withMenuPage(1)
      val sorting = mSorting.right.get

      if (sorting.isEmpty) {
        newUser = newUser.withEmptySorting()
      } else {
        newUser = newUser.withAddedSorting(sorting.get)
      }
      db.updateUser(newUser)
      responseService.mkChangeSortingResponse(newUser.state.sorting)
    } else {
      responseService.mkChangeSortingResponse(user.state.sorting)
    }
  }

  override def toggleNotifications()(implicit user: User): (String, ReplyMarkup) = {
    val updated = db.updateUser(user.withNotificationsEnabled(!user.state.notificationsEnabled))
    responseService.mkToggleNotificationsResponse(updated.state.notificationsEnabled)
  }
}
