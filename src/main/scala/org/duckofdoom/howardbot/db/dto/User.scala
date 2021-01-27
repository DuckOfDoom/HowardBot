package org.duckofdoom.howardbot.db.dto

import org.duckofdoom.howardbot.bot.utils.Sorting._

case class User(
    id: Long,
    userId: Int,
    firstName: String,
    lastName: Option[String],
    username: Option[String],
    state: UserState,
    cart: Map[Int, Int]
) {

  def withMenuPage(page: Int): User = {
    copy(state = state.copy(menuPage = Math.max(1, page)))
  }

  def withStylesPage(page: Int): User = {
    copy(state = state.copy(stylesPage = Math.max(1, page)))
  }

  def withAddedSorting(sorting: Sorting): User = {
    copy(state = state.copy(sorting = state.sorting :+ sorting))
  }

  def withEmptySorting(): User = {
    copy(state = state.copy(sorting = Seq()))
  }

  def withNotificationsEnabled(enabled: Boolean): User = {
    copy(state = state.copy(notificationsEnabled = enabled))
  }

  override def toString: String = s"@${username.getOrElse(userId.toString + s" ($firstName)")}"
}
