package org.duckofdoom.howardbot.bot

//import io.circe.{Decoder, Encoder, HCursor, Json}
//import io.circe.syntax._
//import cats.syntax.either._
//import cats.syntax.functor._

object CallbackUtils {

  object CallbackType extends Enumeration {
    type CallbackType = Value
    val Menu: CallbackUtils.CallbackType.Value         = Value("Menu")
    val Styles: CallbackUtils.CallbackType.Value       = Value("Styles")
    val ItemsByStyle: CallbackUtils.CallbackType.Value = Value("ItemsByStyle")
  }
}

sealed abstract class Callback extends Product with Serializable

object Callback {

  // Due to restriction of 64 bytes, we have to compress this json.
  // Prob should move to binary serialization
  // n - new message
  // p - page
  // id - style id
  final case class Menu(p: Option[Int], n: Boolean) extends Callback
  final case class Styles(p: Option[Int], n: Boolean) extends Callback
  final case class ItemsByStyle(id: Int, p: Int) extends Callback
  
//  private implicit val menuEncoder: Encoder[Menu] = (m: Menu) =>
//    Json.obj(
//      ("t", "menu")
//      ("p", if (m.page.isDefined) Json.fromInt(m.page.get) else Json.Null),
//      ("n", Json.fromBoolean(m.newMessage))
//  )
//
//  private implicit val menuDecoder: Decoder[Menu] = (c: HCursor) => {
//    val page: Option[Int]   = c.downField("p").as[Option[Int]].getOrElse(None)
//    val newMessage: Boolean = c.downField("n").as[Boolean].getOrElse(false)
//    Menu(page, newMessage).asRight
//  }
//
//  private implicit val stylesEncoder: Encoder[Styles] = (st: Styles) =>
//    Json.obj(
//      ("p", if (st.page.isDefined) Json.fromInt(st.page.get) else Json.Null),
//      ("n", Json.fromBoolean(st.newMessage))
//  )
//
//  private implicit val stylesDecoder: Decoder[Styles] = (c: HCursor) => {
//    val page: Option[Int]   = c.downField("p").as[Option[Int]].getOrElse(None)
//    val newMessage: Boolean = c.downField("n").as[Boolean].getOrElse(false)
//    Styles(page, newMessage).asRight
//  }
//
//  private implicit val itemsByStyleEncoder: Encoder[ItemsByStyle] = (ibs: ItemsByStyle) =>
//    Json.obj(
//      ("s", Json.fromInt(ibs.styleId)),
//      ("p", Json.fromInt(ibs.page))
//  )
//
//  private implicit val itemsByStyleDecoder: Decoder[ItemsByStyle] = (c: HCursor) => {
//    val styleId: Int = c.downField("p").as[Int].right.get
//    val page: Int    = c.downField("n").as[Int].right.get
//    ItemsByStyle(styleId, page).asRight
//  }
//
//  // ADT Encoding/Decoding as per circe docs
//  implicit val encodeCallback: Encoder[Callback] = Encoder.instance {
//    case menu @ Menu(_, _)                 => menu.asJson
//    case styles @ Styles(_, _)             => styles.asJson
//    case itemsByStyle @ ItemsByStyle(_, _) => itemsByStyle.asJson
//  }
//
//  implicit val decodeCallback: Decoder[Callback] =
//    List[Decoder[Callback]](
//      Decoder[Menu].widen,
//      Decoder[Styles].widen,
//      Decoder[ItemsByStyle].widen,
//    ).reduceLeft(_ or _)
}
