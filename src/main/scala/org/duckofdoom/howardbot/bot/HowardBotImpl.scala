package org.duckofdoom.howardbot.bot

import com.bot4s.telegram.models.ReplyMarkup
import org.duckofdoom.howardbot.bot.data.{Beer, ItemType}
import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.bot.services.{ItemsProvider, ResponseService}
import org.duckofdoom.howardbot.bot.utils.Sorting
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User

import scala.collection.mutable.ListBuffer

class HowardBotImpl(responseService: ResponseService, itemsProvider: ItemsProvider, db: DB) extends HowardBot {

  override def showMenu(page: Option[Int] = None)(implicit user: User): Response = {
    val userState = page
      .map(p => db.updateUser(user.withMenuPage(p)))
      .getOrElse(user)
      .state

    val beers = Sorting.sort(itemsProvider.availableBeers, userState.sorting).toList
    responseService.mkMenuResponse(beers, userState.menuPage)
  }

  override def showStyles(page: Option[Int] = None)(implicit user: User): Response = {
    val stylesPage = page
      .map(p => db.updateUser(user.withMenuPage(p)))
      .getOrElse(user)
      .state
      .stylesPage

    val availableStyles = itemsProvider.getAvailableStyles(true)
    val stylesWithCounts = availableStyles.zip(
      availableStyles.map { st =>
        itemsProvider.findBeerByStyleId(st.id).length
      }
    )

    val stylesWithCountsMap = stylesWithCounts.toMap
    responseService.mkStylesResponse(stylesWithCountsMap, stylesPage)
  }

  override def showBeersByStyle(styleId: Int, page: Option[Int])(implicit user: User): Response = {
    val beers = Sorting.sort(itemsProvider.findBeerByStyleId(styleId), user.state.sorting).toList
    responseService.mkBeersByStyleResponse(styleId, beers, page.getOrElse(1))
  }

  override def showItem(itemType: ItemType, itemId: Int)(implicit user: User): Response = {
    itemType match {
      case ItemType.Beer  => responseService.mkBeerResponse(itemId)
      case ItemType.Style => {
        val beers = Sorting.sort(itemsProvider.findBeerByStyleId(itemId), user.state.sorting).toList
        responseService.mkBeersByStyleResponse(itemId, beers, 1)
      }
    }
  }

  override def search(query: String, page: Option[Int])(implicit user: User): (String, ReplyMarkup) = {
    val beers = ListBuffer[Beer]()
    beers.appendAll(itemsProvider.availableBeers)

    def findAndRemove(filter: Beer => Boolean): Seq[Beer] = {
      val results = beers.filter(b => filter(b))
      results.foreach(b => beers -= b)
      results
    }

    val q                = query.toLowerCase()
    val resultsByName    = findAndRemove(b => b.name.exists(n => n.toLowerCase.contains(q)))
    val resultsByBrewery = findAndRemove(b => b.breweryInfo.name.exists(n => n.toLowerCase.contains(q)))
    val resultsByStyle   = findAndRemove(b => b.style.exists(n => n.toLowerCase.contains(q)))

    val searchResults = resultsByName ++ resultsByBrewery ++ resultsByStyle

    if (searchResults.nonEmpty)
      responseService.mkSearchResponse(query, searchResults, page.getOrElse(1))
    else
      responseService.mkEmptySearchResultsResponse(query)
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
