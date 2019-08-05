package org.duckofdoom.howardbot.utils

object PaginationUtils {
  def mkFirst(page: Int) = s"<< $page"
  def mkLast(page: Int) = s"$page >>"
  def mkPrev(page: Int) = s"< $page"
  def mkNext(page: Int) = s"$page >"
  def mkCurr(page: Int) = s"* $page *"
  def mkNormal(page: Int) = page.toString
}
