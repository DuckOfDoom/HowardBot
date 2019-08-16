package org.duckofdoom.howardbot.utils

import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardButton
import io.circe.generic.auto._
import io.circe.syntax._
import org.duckofdoom.howardbot.bot.Callback
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType.CallbackType
import slogging.LazyLogging

import scala.collection.mutable

case class Button(text: String, callback: String)

object PaginationUtils extends LazyLogging {

  def mkFirst(page: Int)          = s"<< $page"
  def mkLast(page: Int)           = s"$page >>"
  def mkPrev(page: Int)           = s"< $page"
  def mkNext(page: Int)           = s"$page >"
  def mkCurr(page: Int)           = s"* $page *"
  def mkNormal(page: Int): String = page.toString

  def mkCallbackData[TPayload](
      page: Option[Int],
      payload: Option[TPayload],
      newMessage: Boolean = false)(implicit callbackType: CallbackType): String = {

    val c = callbackType match {
      case CallbackType.Menu   => Callback.Menu(page, newMessage)
      case CallbackType.Styles => Callback.Styles(page, newMessage)
      case CallbackType.ItemsByStyle =>
        require(page.isDefined, "Can't make ItemsByStyle callback without a page.")
        require(payload.isInstanceOf[Option[Int]])
        Callback.ItemsByStyle(payload.asInstanceOf[Option[Int]].get, page.get)
    }

    val callbackData = c.asJson.toString

    // As per Telegram API requirements
    if (callbackData.getBytes.length > 64)
      logger.error(s"Callback data '$callbackData' is too long! Must be below 64 bytes!")

    callbackData
  }

  def mkAdditionalButtons(menu: Boolean, styles: Boolean): Seq[InlineKeyboardButton] = {
    var buttonsList = mutable.MutableList[InlineKeyboardButton]()
    if (menu) {
      buttonsList += InlineKeyboardButton.callbackData(
        Locale.menu,
        mkCallbackData(None, None)(CallbackType.Menu))
    }

    if (styles) {
      buttonsList += InlineKeyboardButton.callbackData(
        Locale.styles,
        mkCallbackData(None, None)(CallbackType.Styles))
    }

    buttonsList
  }

  def mkButtonsForPaginatedQuery[TPayload](currentPage: Int,
                                           itemsPerPage: Int,
                                           itemsCount: Int,
                                           payload: Option[TPayload])(
      implicit callbackType: CallbackType
  ): Seq[Button] = {

    var list       = new mutable.MutableList[Button]
    var totalPages = itemsCount / itemsPerPage
    if (itemsCount % itemsPerPage != 0)
      totalPages += 1

    if (totalPages <= 1) {
      return List()
    } else if (totalPages <= 5) {
      for (page <- 1 to totalPages) {
        list += Button(if (page == currentPage) mkCurr(page) else mkNormal(page),
                       mkCallbackData(page.some, payload))
      }
    } else {
      if (currentPage >= 1 && currentPage <= 3) {
        currentPage match {
          case 1 =>
            list += Button(mkCurr(currentPage), mkCallbackData(currentPage.some, payload))
            list += Button(mkNormal(currentPage + 1), mkCallbackData((currentPage + 1).some, payload))
            list += Button(mkNormal(currentPage + 2), mkCallbackData((currentPage + 2).some, payload))
            list += Button(mkNext(currentPage + 3), mkCallbackData((currentPage + 3).some, payload))
            list += Button(mkLast(totalPages), mkCallbackData(totalPages.some, payload))
          case 2 =>
            list += Button(mkNormal(currentPage - 1), mkCallbackData((currentPage - 1).some, payload))
            list += Button(mkCurr(currentPage), mkCallbackData(currentPage.some, payload))
            list += Button(mkNormal(currentPage + 1), mkCallbackData((currentPage + 1).some, payload))
            list += Button(mkNext(currentPage + 2), mkCallbackData((currentPage + 2).some, payload))
            list += Button(mkLast(totalPages), mkCallbackData(totalPages.some, payload))
          case 3 =>
            list += Button(mkNormal(currentPage - 2), mkCallbackData((currentPage - 2).some, payload))
            list += Button(mkNormal(currentPage - 1), mkCallbackData((currentPage - 1).some, payload))
            list += Button(mkCurr(currentPage), mkCallbackData(currentPage.some, payload))
            list += Button(mkNext(currentPage + 1), mkCallbackData((currentPage + 1).some, payload))
            list += Button(mkLast(totalPages), mkCallbackData(totalPages.some, payload))
        }
      } else if (currentPage >= totalPages - 3 && currentPage <= totalPages) {

        if (currentPage == totalPages - 3) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, payload))
          list += Button(PaginationUtils.mkPrev(currentPage - 1),
                         PaginationUtils.mkCallbackData((currentPage - 1).some, payload))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallbackData(currentPage.some, payload))
          list += Button(PaginationUtils.mkNext(currentPage + 1),
                         PaginationUtils.mkCallbackData((currentPage + 1).some, payload))
          list += Button(PaginationUtils.mkLast(totalPages),
                         PaginationUtils.mkCallbackData(totalPages.some, payload))
        } else if (currentPage == totalPages - 2) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, payload))
          list += Button(PaginationUtils.mkPrev(currentPage - 1),
                         PaginationUtils.mkCallbackData((currentPage - 1).some, payload))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallbackData(currentPage.some, payload))
          list += Button(PaginationUtils.mkNext(currentPage + 1),
                         PaginationUtils.mkCallbackData((currentPage + 1).some, payload))
          list += Button(PaginationUtils.mkLast(totalPages),
                         PaginationUtils.mkCallbackData(totalPages.some, payload))
        } else if (currentPage == totalPages - 1) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, payload))
          list += Button(PaginationUtils.mkPrev(currentPage - 2),
                         PaginationUtils.mkCallbackData((currentPage - 2).some, payload))
          list += Button(PaginationUtils.mkNormal(currentPage - 1),
                         PaginationUtils.mkCallbackData((currentPage - 1).some, payload))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallbackData(currentPage.some, payload))
          list += Button(PaginationUtils.mkNormal(totalPages),
                         PaginationUtils.mkCallbackData(totalPages.some, payload))
        } else if (currentPage == totalPages) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, payload))
          list += Button(PaginationUtils.mkPrev(currentPage - 3),
                         PaginationUtils.mkCallbackData((currentPage - 3).some, payload))
          list += Button(PaginationUtils.mkNormal(currentPage - 2),
                         PaginationUtils.mkCallbackData((currentPage - 2).some, payload))
          list += Button(PaginationUtils.mkNormal(currentPage - 1),
                         PaginationUtils.mkCallbackData((currentPage - 1).some, payload))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallbackData(currentPage.some, payload))
        }

      } else {
        list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, payload))
        list += Button(PaginationUtils.mkPrev(currentPage - 1),
                       PaginationUtils.mkCallbackData((currentPage - 1).some, payload))
        list += Button(PaginationUtils.mkNormal(currentPage),
                       PaginationUtils.mkCallbackData(currentPage.some, payload))
        list += Button(PaginationUtils.mkNext(currentPage + 1),
                       PaginationUtils.mkCallbackData((currentPage + 1).some, payload))
        list += Button(PaginationUtils.mkLast(totalPages),
                       PaginationUtils.mkCallbackData(totalPages.some, payload))
      }
    }

    list.toList
  }
}
