package org.duckofdoom.howardbot.bot.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import cats.syntax.either._
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.bot.data.{Beer, Item, ItemType, Style}
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.utils.Extensions.AnyRefExtensions
import slogging.StrictLogging

sealed abstract class Callback extends Product with Serializable

object Callback extends Enumeration with StrictLogging {

  object Type extends Enumeration {
    val Menu: Type.Value                = Value("Menu")
    val Styles: Type.Value              = Value("Styles")
    val ItemsByStyle: Type.Value        = Value("ItemsByStyle")
    val SingleBeer: Type.Value          = Value("SingleBeer")
    val SearchBeerByName: Type.Value    = Value("SearchBeerByName")
    val SearchBeerByStyle: Type.Value   = Value("SearchBeerByStyle")
    val Settings: Type.Value            = Value("Settings")
    val ChangeSorting: Type.Value       = Value("ChangeSorting")
    val ToggleNotifications: Type.Value = Value("ToggleNotifications")
  }

  final case class Menu(page: Option[Int], newMessage: Boolean)   extends Callback
  final case class Styles(page: Option[Int], newMessage: Boolean) extends Callback
  final case class ItemsByStyle(styleId: Int, page: Int)          extends Callback
  final case class SingleItem(itemType: ItemType, itemId: Int)    extends Callback
  final case class SearchBeerByName(query: String, page: Int)     extends Callback
  final case class SearchBeerByStyle(query: String, page: Int)    extends Callback
  final case class Settings()                                     extends Callback

  // TODO: Either[Unit, Option[Sorting]] is not a good type. Need to rewrite this using ADT.
  final case class ChangeSorting(sorting: Either[Unit, Option[Sorting]]) extends Callback
  final case class ToggleNotifications()                                 extends Callback

  // two 127 bytes
  private final val SerializeNoneValue = Array(126, 127).map(_.toByte)
  private final val ResetSortingValue  = -1

  def mkMenuCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Menu(page, newMessage))
  }

  def mkStylesCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Styles(page, newMessage))
  }

  def mkItemsByStyleCallbackData(styleId: Int, page: Int): String = {
    serializeCallback(Callback.ItemsByStyle(styleId, page))
  }

  def mkSettingsCallback(): String = {
    serializeCallback(Callback.Settings())
  }

  def mkChangeSortingCallback(sorting: Either[Unit, Option[Sorting]]): String = {
    serializeCallback(Callback.ChangeSorting(sorting))
  }

  def mkToggleNotificationsCallback(): String = {
    serializeCallback(Callback.ToggleNotifications())
  }

  def mkSingleItemCallback[A <: Item](item: A): Option[String] = {
    val itemType = item match {
      case _: Beer  => ItemType.Beer
      case _: Style => ItemType.Style
      case _ =>
        logger.error(s"Unknown type for item $item")
        return None
    }

    serializeCallback(Callback.SingleItem(itemType, item.id)).some
  }

  def mkSearchBeerByNameCallback(query: String, page: Int): String = {
    serializeCallback(Callback.SearchBeerByName(query, page))
  }

  def mkSearchBeerByStyleCallback(query: String, page: Int): String = {
    serializeCallback(Callback.SearchBeerByStyle(query, page))
  }

  private def serializeCallback(callbackData: Callback): String = {
    callbackData
      .serialize()
      // As per Telegram API requirements
      .check(
        _.length < 64,
        _ => logger.error(s"Callback data '$callbackData' is too long! Must be below 64 bytes!")
      )
      .map(_.toChar)
      .mkString
  }

  implicit class SerializableCallback(c: Callback) {

    def serialize(): Array[Byte] = {

      def writeOption(stream: DataOutputStream, option: Option[Int]): Unit = {
        if (option.isDefined)
          stream.writeShort(option.get)
        else
          stream.write(SerializeNoneValue)
      }

      val byteArrayStream = new ByteArrayOutputStream()
      val stream          = new DataOutputStream(byteArrayStream)
      c match {
        case Menu(page, newMessage) =>
          stream.writeShort(1)
          writeOption(stream, page)
          stream.writeBoolean(newMessage)
        case Styles(page, newMessage) =>
          stream.writeShort(2)
          writeOption(stream, page)
          stream.writeBoolean(newMessage)
        case ItemsByStyle(styleId, page) =>
          stream.writeShort(3)
          stream.writeShort(styleId)
          stream.writeShort(page)
        case SingleItem(itemType, itemId) =>
          stream.writeShort(4)
          stream.writeShort(itemType.id)
          stream.writeShort(itemId)
        case SearchBeerByName(query, page) =>
          stream.writeShort(5)
          stream.writeUTF(query)
          stream.writeShort(page)
        case SearchBeerByStyle(query, page) =>
          stream.writeShort(6)
          stream.writeUTF(query)
          stream.writeShort(page)
        case Settings() =>
          stream.writeShort(7)
        case ChangeSorting(eitherSortingOrNothing) =>
          stream.writeShort(8)
          stream.writeBoolean(eitherSortingOrNothing.isLeft)

          if (eitherSortingOrNothing.isRight)
            stream.writeShort(
              eitherSortingOrNothing.right.get.map(_.id).getOrElse(ResetSortingValue)
            )
        case ToggleNotifications() =>
          stream.writeShort(9)
      }

      byteArrayStream.toByteArray
    }
  }

  def deserialize[A <: Callback](bytes: Array[Byte]): Option[Callback] = {

    def shortToOption(sh: Short) = {
      // I'm sorry Jon
      if (Array((sh >> 8) & 0xFF, (sh >> 0) & 0xFF) sameElements SerializeNoneValue) None else sh.toInt.some
    }

    try {
      val stream = new DataInputStream(new ByteArrayInputStream(bytes))
      val result = stream.readUnsignedShort() match {
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
          SingleItem(itemType, itemId)
        case 5 =>
          val query = stream.readUTF()
          val page  = stream.readShort()
          SearchBeerByName(query, page)
        case 6 =>
          val query = stream.readUTF()
          val page  = stream.readShort()
          SearchBeerByStyle(query, page)
        case 7 => Settings()
        case 8 =>
          val hasSorting = !stream.readBoolean()
          if (!hasSorting) {
            ChangeSorting(().asLeft)
          } else {
            val sorting = stream.readShort()
            ChangeSorting(Sorting.all.find(s => s.id == sorting).asRight)
          }
        case 9 => ToggleNotifications()
      }

      result.some
    } catch {
      case e: Throwable =>
        logger.error("Failed to deserialize callback", e)
        None
    }
  }
}
