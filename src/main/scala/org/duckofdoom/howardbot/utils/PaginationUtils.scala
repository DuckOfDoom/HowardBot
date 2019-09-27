package org.duckofdoom.howardbot.utils

import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardButton
import slogging.LazyLogging

import scala.collection.mutable

object PaginationUtils extends LazyLogging {

  def mkFirst(page: Int)          = s"<< $page"
  def mkLast(page: Int)           = s"$page >>"
  def mkPrev(page: Int)           = s"< $page"
  def mkNext(page: Int)           = s"$page >"
  def mkCurr(page: Int)           = s"* $page *"
  def mkNormal(page: Int): String = page.toString

  def mkButtonsForPaginatedQuery[TPayload](currentPage: Int,
                                           itemsPerPage: Int,
                                           itemsCount: Int,
                                           mkCallbackData:Int => String
                                          )
  : Seq[InlineKeyboardButton] = {

    var list       = new mutable.ListBuffer[InlineKeyboardButton]
    var totalPages = itemsCount / itemsPerPage
    if (itemsCount % itemsPerPage != 0)
      totalPages += 1

    if (totalPages <= 1) {
      return List()
    } else if (totalPages <= 5) {
      for (page <- 1 to totalPages) {
        list += InlineKeyboardButton(if (page == currentPage) mkCurr(page) else mkNormal(page),
                       mkCallbackData(page).some)
      }
    } else {
      if (currentPage >= 1 && currentPage <= 3) {
        currentPage match {
          case 1 =>
            list += InlineKeyboardButton(mkCurr(currentPage), mkCallbackData(currentPage).some)
            list += InlineKeyboardButton(mkNormal(currentPage + 1),
                           mkCallbackData(currentPage + 1).some)
            list += InlineKeyboardButton(mkNormal(currentPage + 2),
                           mkCallbackData(currentPage + 2).some)
            list += InlineKeyboardButton(mkNext(currentPage + 3), mkCallbackData(currentPage + 3).some)
            list += InlineKeyboardButton(mkLast(totalPages), mkCallbackData(totalPages).some)
          case 2 =>
            list += InlineKeyboardButton(mkNormal(currentPage - 1),
                           mkCallbackData(currentPage - 1).some)
            list += InlineKeyboardButton(mkCurr(currentPage), mkCallbackData(currentPage).some)
            list += InlineKeyboardButton(mkNormal(currentPage + 1),
                           mkCallbackData(currentPage + 1).some)
            list += InlineKeyboardButton(mkNext(currentPage + 2), mkCallbackData(currentPage + 2).some)
            list += InlineKeyboardButton(mkLast(totalPages), mkCallbackData(totalPages).some)
          case 3 =>
            list += InlineKeyboardButton(mkNormal(currentPage - 2),
                           mkCallbackData(currentPage - 2).some)
            list += InlineKeyboardButton(mkNormal(currentPage - 1),
                           mkCallbackData(currentPage - 1).some)
            list += InlineKeyboardButton(mkCurr(currentPage), mkCallbackData(currentPage).some)
            list += InlineKeyboardButton(mkNext(currentPage + 1), mkCallbackData(currentPage + 1).some)
            list += InlineKeyboardButton(mkLast(totalPages), mkCallbackData(totalPages).some)
        }
      } else if (currentPage >= totalPages - 3 && currentPage <= totalPages) {

        if (currentPage == totalPages - 3) {
          list += InlineKeyboardButton(PaginationUtils.mkFirst(1),
                         mkCallbackData(1).some)
          list += InlineKeyboardButton(PaginationUtils.mkPrev(currentPage - 1),
                         mkCallbackData(currentPage - 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkCurr(currentPage),
                         mkCallbackData(currentPage).some)
          list += InlineKeyboardButton(PaginationUtils.mkNext(currentPage + 1),
                         mkCallbackData(currentPage + 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
                         mkCallbackData(totalPages).some)
        } else if (currentPage == totalPages - 2) {
          list += InlineKeyboardButton(PaginationUtils.mkFirst(1),
                         mkCallbackData(1).some)
          list += InlineKeyboardButton(PaginationUtils.mkPrev(currentPage - 1),
                         mkCallbackData(currentPage - 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkCurr(currentPage),
                         mkCallbackData(currentPage).some)
          list += InlineKeyboardButton(PaginationUtils.mkNext(currentPage + 1),
                         mkCallbackData(currentPage + 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
                         mkCallbackData(totalPages).some)
        } else if (currentPage == totalPages - 1) {
          list += InlineKeyboardButton(PaginationUtils.mkFirst(1),
                         mkCallbackData(1).some)
          list += InlineKeyboardButton(PaginationUtils.mkPrev(currentPage - 2),
                         mkCallbackData(currentPage - 2).some)
          list += InlineKeyboardButton(PaginationUtils.mkNormal(currentPage - 1),
                         mkCallbackData(currentPage - 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkCurr(currentPage),
                         mkCallbackData(currentPage).some)
          list += InlineKeyboardButton(PaginationUtils.mkNormal(totalPages),
                         mkCallbackData(totalPages).some)
        } else if (currentPage == totalPages) {
          list += InlineKeyboardButton(PaginationUtils.mkFirst(1),
                         mkCallbackData(1).some)
          list += InlineKeyboardButton(PaginationUtils.mkPrev(currentPage - 3),
                         mkCallbackData(currentPage - 3).some)
          list += InlineKeyboardButton(PaginationUtils.mkNormal(currentPage - 2),
                         mkCallbackData(currentPage - 2).some)
          list += InlineKeyboardButton(PaginationUtils.mkNormal(currentPage - 1),
                         mkCallbackData(currentPage - 1).some)
          list += InlineKeyboardButton(PaginationUtils.mkCurr(currentPage),
                         mkCallbackData(currentPage).some)
        }

      } else {
        list += InlineKeyboardButton(PaginationUtils.mkFirst(1), mkCallbackData(1).some)
        list += InlineKeyboardButton(PaginationUtils.mkPrev(currentPage - 1),
                       mkCallbackData(currentPage - 1).some)
        list += InlineKeyboardButton(PaginationUtils.mkNormal(currentPage),
                       mkCallbackData(currentPage).some)
        list += InlineKeyboardButton(PaginationUtils.mkNext(currentPage + 1),
                       mkCallbackData(currentPage + 1).some)
        list += InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
                       mkCallbackData(totalPages).some)
      }
    }

    list.toList
  }

}
