package org.duckofdoom.howardbot.bot

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardButton
import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemType, Style}
import org.duckofdoom.howardbot.utils.Extensions.AnyRefExtensions
import org.duckofdoom.howardbot.utils.Locale
import slogging.StrictLogging

import scala.collection.mutable

object CallbackUtils extends StrictLogging {

  object CallbackType extends Enumeration {
    type CallbackType = Value
    val Menu: CallbackUtils.CallbackType.Value         = Value("Menu")
    val Styles: CallbackUtils.CallbackType.Value       = Value("Styles")
    val ItemsByStyle: CallbackUtils.CallbackType.Value = Value("ItemsByStyle")
    val Item: CallbackUtils.CallbackType.Value         = Value("Item")
  }

  def mkMenuCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Menu(page, newMessage))
  }

  def mkStylesCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Styles(page, newMessage))
  }

  def mkItemsByStyleCallbackData(styleId: Int, page: Int): String = {
    serializeCallback(Callback.ItemsByStyle(styleId, page))
  }

  def mkItemCallback[A <: Item](item: A): Option[String] = {
    val itemType = item match {
      case _: Beer  => ItemType.Beer
      case _: Style => ItemType.Style
      case _ =>
        logger.error(s"Unknown type for item $item")
        return None
    }

    serializeCallback(Callback.Item(itemType, item.id)).some
  }

  def mkAdditionalButtons(menu: Boolean, styles: Boolean): Seq[InlineKeyboardButton] = {
    var buttonsList = mutable.MutableList[InlineKeyboardButton]()
    if (menu) {
      buttonsList += InlineKeyboardButton.callbackData(
        Locale.menu,
        mkMenuCallbackData(None, newMessage = false)
      )
    }

    if (styles) {
      buttonsList += InlineKeyboardButton.callbackData(
        Locale.styles,
        mkStylesCallbackData(None, newMessage = false)
      )
    }

    buttonsList
  }

  private def serializeCallback(callbackData: Callback): String = {
    callbackData
      .serialize()
      // As per Telegram API requirements
      .check(
        _.length < 64,
        _ => logger.error(s"Callback data '$callbackData' is too long! Must be below 64 bytes!")
      )
      .map(_.asInstanceOf[Char])
      .mkString
  }
}

sealed abstract class Callback extends Product with Serializable

object Callback extends StrictLogging {

  final case class Menu(page: Option[Int], newMessage: Boolean)   extends Callback
  final case class Styles(page: Option[Int], newMessage: Boolean) extends Callback
  final case class ItemsByStyle(styleId: Int, page: Int)          extends Callback
  final case class Item(itemType: ItemType, itemId: Int)          extends Callback

  implicit class SerializableCallback(c: Callback) {

    def serialize(): Array[Byte] = {
      val byteArrayInputStream = new ByteArrayOutputStream()
      val stream               = new DataOutputStream(byteArrayInputStream)
      c match {
        case Menu(page, newMessage) =>
          stream.writeShort(1)
          stream.writeShort(page.getOrElse(-1).asInstanceOf[Short])
          stream.writeBoolean(newMessage)
        case Styles(page, newMessage) =>
          stream.writeShort(2)
          stream.writeShort(page.getOrElse(-1).asInstanceOf[Short])
          stream.writeBoolean(newMessage)
        case ItemsByStyle(styleId, page) =>
          stream.writeShort(3)
          stream.writeShort(styleId)
          stream.writeShort(page)
        case Item(itemType, itemId) =>
          stream.writeShort(4)
          stream.writeShort(itemType.id)
          stream.writeShort(itemId)
      }

      byteArrayInputStream.toByteArray
    }
  }

  def deserialize[A <: Callback](bytes: Array[Byte]): Option[Callback] = {
    def shortToOption(i: Int) = {
      if (i >= 0) i.asInstanceOf[Int].some else None
    }

    try {
      val stream = new DataInputStream(new ByteArrayInputStream(bytes))
      val result = stream.readShort() match {
        case 1 =>
          val page       = shortToOption(stream.readShort())
          val newMessage = stream.readBoolean()
          Menu(page, newMessage)
        case 2 =>
          val page       = shortToOption(stream.readShort())
          val newMessage = stream.readBoolean()
          Styles(page, newMessage)
        case 3 =>
          val styleId = stream.readShort()
          val page    = stream.readShort()
          ItemsByStyle(styleId, page)
        case 4 =>
          val itemType = ItemType(stream.readShort())
          val itemId   = stream.readShort()
          Item(itemType, itemId)
      }

      result.some

    } catch {
      case e: Throwable =>
        logger.error("Failed to deserialize callback", e)
        None
    }
  }
}
