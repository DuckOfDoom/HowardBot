package org.duckofdoom.howardbot.db.dto

import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import cats.syntax.either._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.duckofdoom.howardbot.bot.utils.Sorting

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