package org.duckofdoom.howardbot.db.dto

import cats.syntax.either._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class UserState(var menuPage: Int = 1)

object UserState {

  implicit val encoder: Encoder[UserState] = (a: UserState) =>
    Json.obj(
      ("menuPage", Json.fromInt(a.menuPage))
  )

  // This decoder should not fail ever!
  implicit val decoder: Decoder[UserState] = (c: HCursor) => {
    val menuPage: Int = c.downField("menuPage").as[Int].getOrElse(1)
    UserState(menuPage).asRight
  }
}

case class User(id: Long,
                userId: Int,
                firstName: String,
                lastName: String,
                username: String,
                state: UserState)
