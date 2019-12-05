package org.duckofdoom.howardbot.db.dto

import cats.syntax.either._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.duckofdoom.howardbot.bot.utils.Sorting
import org.duckofdoom.howardbot.bot.utils.Sorting._

case class UserState(
    menuPage: Int = 1,
    stylesPage: Int = 1,
    notificationsEnabled: Boolean = true,
    sorting: Seq[Sorting] = Seq()
)

object UserState {

  implicit val encoder: Encoder[UserState] = (state: UserState) =>
    Json.obj(
      // Menu page
      ("mp", Json.fromInt(state.menuPage)),
      // Styles page
      ("sp", Json.fromInt(state.stylesPage)),
      // Notifications enabled?
      ("n", Json.fromBoolean(state.notificationsEnabled)),
      // Sorting
      ("s", Json.fromValues(state.sorting.map(s => Json.fromInt(s.id))))
    )

  // This decoder should not fail ever!
  implicit val decoder: Decoder[UserState] = (c: HCursor) => {
    val menuPage: Int                    = c.downField("mp").as[Int].getOrElse(1)
    val stylesPage: Int                  = c.downField("sp").as[Int].getOrElse(1)
    val notificationsEnabled: Boolean    = c.downField("n").as[Boolean].getOrElse(true)
    val sortingDecoder: Decoder[Sorting] = Decoder.decodeInt.map(i => Sorting(i))

    val sorting: Seq[Sorting] = c
      .downField("s")
      .values
      .getOrElse(List())
      .map(v => sortingDecoder.decodeJson(v).right.get)
      .toList

    UserState(menuPage, stylesPage, notificationsEnabled, sorting).asRight
  }
}

case class User(id: Long, userId: Int, firstName: String, lastName: String, username: String, state: UserState) {

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
}
