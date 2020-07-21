package org.duckofdoom.howardbot.db

import doobie.{Get, Put}
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.Decoder
import org.duckofdoom.howardbot.db.dto.UserState
import slogging.StrictLogging

object DBDataUtils extends StrictLogging {

  implicit val getCart : Get[Map[Int, Int]] = Get[String].tmap(str => {
    decode[Map[Int, Int]](str) match {
      case Left(err) =>
        logger.error(s"Failed to deserialize user cart '$str'\n$err.\nReturning empty cart.")
        Map[Int, Int]()
      case Right(st) => st
    }
  })

  implicit val putCart : Put[Map[Int, Int]] = Put[String].tcontramap(_.asJson.toString)

  implicit val getUserState: Get[UserState] = Get[String].tmap(str => {

    implicit val decoder: Decoder[UserState] = UserState.decoder

    decode[UserState](str) match {
      case Left(err) =>
        logger.error(s"Failed to deserialize user state '$str'\n$err.\nReturning default state.")
        UserState()
      case Right(st) => st
    }
  })

  implicit val putUserState: Put[UserState] = Put[String].tcontramap(_.asJson.toString)
}
