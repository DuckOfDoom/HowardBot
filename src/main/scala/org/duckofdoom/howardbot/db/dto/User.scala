package org.duckofdoom.howardbot.db.dto

import io.circe.{Decoder, Encoder, HCursor, Json}

case class User(id: Long, userId: Int, firstName:String, lastName: String, username:String, state: UserState)

case class UserState(var menuPage: Int = 1) {
  
  implicit val encoder: Encoder[UserState] = (a: UserState) => Json.obj(
    ("menuPage", Json.fromInt(a.menuPage))
  )

  implicit val decodeFoo: Decoder[UserState] = (c: HCursor) => {
    for {
      foo <- c.downField("menuPage").as[Int]
    } yield {
      UserState(foo)
    }
  }
}

