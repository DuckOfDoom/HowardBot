package org.duckofdoom.howardbot.bot

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
}
