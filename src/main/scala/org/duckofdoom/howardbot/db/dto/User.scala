package org.duckofdoom.howardbot.db.dto

import cats.syntax.either._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.duckofdoom.howardbot.bot.Sorting
import org.duckofdoom.howardbot.bot.Sorting.Sorting
 
case class UserState(menuPage: Int = 1, stylesPage: Int = 1, sorting: Seq[Sorting] = Seq())

object UserState {

  implicit val encoder: Encoder[UserState] = (a: UserState) =>
    Json.obj(
      ("menuPage", Json.fromInt(a.menuPage)),
      ("stylesPage", Json.fromInt(a.stylesPage)),
      ("sorting", Json.fromValues(a.sorting.map(s => Json.fromInt(s.id))))
  )

  // This decoder should not fail ever!
  implicit val decoder: Decoder[UserState] = (c: HCursor) => {
    val menuPage: Int   = c.downField("menuPage").as[Int].getOrElse(1)
    val stylesPage: Int = c.downField("stylesPage").as[Int].getOrElse(1)
    val sortingDecoder : Decoder[Sorting] = Decoder.decodeInt.map(i => Sorting(i))
    
    val sorting : List[Sorting] = c.downField("sorting").values
      .getOrElse(List())
      .map(v => sortingDecoder.decodeJson(v).right.get)
      .toList
    
    UserState(menuPage, stylesPage, sorting).asRight
  }
  
}

case class User(id: Long,
                userId: Int,
                firstName: String,
                lastName: String,
                username: String,
                state: UserState) {
  
  def withMenuPage(page: Int): User = { 
    copy(state = state.copy(menuPage = page))
  }
  
  def withStylesPage(page: Int): User = {
    copy(state = state.copy(stylesPage = page))
  }
  
  def withAddedSorting(sorting: Sorting): User = {
    copy(state = state.copy(sorting = state.sorting :+ sorting))
  }
  
  def withEmptySorting(): User = {
    copy(state = state.copy(sorting = Seq()))
  }
}
