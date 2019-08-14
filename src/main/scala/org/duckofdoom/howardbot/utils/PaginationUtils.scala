package org.duckofdoom.howardbot.utils

import org.duckofdoom.howardbot.bot.data.Static
import org.duckofdoom.howardbot.bot.data.Static.CallbackType.CallbackType

import scala.collection.mutable

case class Button(text: String, callback: String)

object PaginationUtils {

  def mkFirst(page: Int)  = s"<< $page"
  def mkLast(page: Int)   = s"$page >>"
  def mkPrev(page: Int)   = s"< $page"
  def mkNext(page: Int)   = s"$page >"
  def mkCurr(page: Int)   = s"* $page *"
  def mkNormal(page: Int): String = page.toString
  def mkCallback(page : Int, values: String*)(implicit callbackType: CallbackType): String = Static.mkCallbackPrefix(values:_*)
  
  def mkButtonsForPaginatedQuery(currentPage: Int, itemsPerPage: Int, itemsCount: Int)(
      implicit callbackType: CallbackType): List[Button] = {
    var list = new mutable.MutableList[Button]

    if (itemsCount <= itemsPerPage) {
      for (i <- 1 to itemsCount) {
        list += Button(if (i == currentPage) mkCurr(i) else mkNormal(i), mkCallback(i))
      }
    } else {
      var totalPages = itemsCount / itemsPerPage
      if (itemsCount % itemsPerPage != 0)
        totalPages += 1

      if (currentPage >= 1 && currentPage <= 3) {
        currentPage match {
          case 1 =>
            list += Button(mkCurr(currentPage), mkCallback(currentPage))
            list += Button(mkNormal(currentPage + 1), mkCallback(currentPage + 1))
            list += Button(mkNormal(currentPage + 2), mkCallback(currentPage + 2))
            list += Button(mkNext(currentPage + 3), mkCallback(currentPage + 3))
            list += Button(mkLast(totalPages), mkCallback(totalPages))
          case 2 =>
            list += Button(mkNormal(currentPage - 1), mkCallback(currentPage - 1))
            list += Button(mkCurr(currentPage), mkCallback(currentPage))
            list += Button(mkNormal(currentPage + 1), mkCallback(currentPage + 1))
            list += Button(mkNext(currentPage + 2), mkCallback(currentPage + 2))
            list += Button(mkLast(totalPages), mkCallback(totalPages))
          case 3 =>
            list += Button(mkNormal(currentPage - 2), mkCallback(currentPage - 2))
            list += Button(mkNormal(currentPage - 1), mkCallback(currentPage - 1))
            list += Button(mkCurr(currentPage), mkCallback(currentPage))
            list += Button(mkNext(currentPage + 1), mkCallback(currentPage + 1))
            list += Button(mkLast(totalPages), mkCallback(totalPages))
        }
      } else if (currentPage >= totalPages - 3 && currentPage <= totalPages) {

        if (currentPage == totalPages - 3) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1))
          list += Button(PaginationUtils.mkPrev(currentPage - 1),
                         PaginationUtils.mkCallback(currentPage - 1))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallback(currentPage))
          list += Button(PaginationUtils.mkNext(currentPage + 1),
                         PaginationUtils.mkCallback(currentPage + 1))
          list += Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
        } else if (currentPage == totalPages - 2) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1))
          list += Button(PaginationUtils.mkPrev(currentPage - 1),
                         PaginationUtils.mkCallback(currentPage - 1))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallback(currentPage))
          list += Button(PaginationUtils.mkNext(currentPage + 1),
                         PaginationUtils.mkCallback(currentPage + 1))
          list += Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
        } else if (currentPage == totalPages - 1) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1))
          list += Button(PaginationUtils.mkPrev(currentPage - 2),
                         PaginationUtils.mkCallback(currentPage - 2))
          list += Button(PaginationUtils.mkNormal(currentPage - 1),
                         PaginationUtils.mkCallback(currentPage - 1))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallback(currentPage))
          list += Button(PaginationUtils.mkNormal(totalPages),
                         PaginationUtils.mkCallback(totalPages))
        } else if (currentPage == totalPages) {
          list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1))
          list += Button(PaginationUtils.mkPrev(currentPage - 3),
                         PaginationUtils.mkCallback(currentPage - 3))
          list += Button(PaginationUtils.mkNormal(currentPage - 2),
                         PaginationUtils.mkCallback(currentPage - 2))
          list += Button(PaginationUtils.mkNormal(currentPage - 1),
                         PaginationUtils.mkCallback(currentPage - 1))
          list += Button(PaginationUtils.mkCurr(currentPage),
                         PaginationUtils.mkCallback(currentPage))
        }

      } else {
        list += Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1))
        list += Button(PaginationUtils.mkPrev(currentPage - 1),
                       PaginationUtils.mkCallback(currentPage - 1))
        list += Button(PaginationUtils.mkNormal(currentPage),
                       PaginationUtils.mkCallback(currentPage))
        list += Button(PaginationUtils.mkNext(currentPage + 1),
                       PaginationUtils.mkCallback(currentPage + 1))
        list += Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
      }
    }

    list.toList
  }
}
