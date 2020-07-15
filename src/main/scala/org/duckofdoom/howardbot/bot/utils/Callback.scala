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
    val Search: Type.Value              = Value("Search")
    val Settings: Type.Value            = Value("Settings")
    val ChangeSorting: Type.Value       = Value("ChangeSorting")
    val ToggleNotifications: Type.Value = Value("ToggleNotifications")
  }

  final case class Menu(page: Option[Int])                       extends Callback
  final case class Styles(page: Option[Int])                     extends Callback
  final case class BeersByStyle(styleId: Int, page: Option[Int]) extends Callback
  final case class SingleBeer(itemType: ItemType, itemId: Int)   extends Callback
  final case class Search(query: String, page: Option[Int])      extends Callback
  final case class Settings()                                    extends Callback

  // TODO: Either[Unit, Option[Sorting]] is not a good type. Need to rewrite this using ADT.
  final case class ChangeSorting(sorting: Either[Unit, Option[Sorting]]) extends Callback
  final case class ToggleNotifications()                                 extends Callback

  // two 127 bytes
  private final val SerializeNoneValue = Array(126, 127).map(_.toByte)
  private final val ResetSortingValue  = -1

  def mkSearchCallback(query: String, page: Option[Int]): String = {
    serializeCallback(Callback.Search(query, page))
  }

  def mkMenuCallbackData(page: Option[Int]): String = {
    serializeCallback(Callback.Menu(page))
  }

  def mkStylesCallbackData(page: Option[Int]): String = {
    serializeCallback(Callback.Styles(page))
  }

  def mkItemsByStyleCallbackData(styleId: Int, page: Option[Int]): String = {
    serializeCallback(Callback.BeersByStyle(styleId, page))
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

    serializeCallback(Callback.SingleBeer(itemType, item.id)).some
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
        case Search(query, page) =>
          stream.writeShort(1)
          stream.writeUTF(query)
          writeOption(stream, page)
        case Menu(page) =>
          stream.writeShort(2)
          writeOption(stream, page)
        case Styles(page) =>
          stream.writeShort(3)
          writeOption(stream, page)
        case BeersByStyle(styleId, page) =>
          stream.writeShort(4)
          stream.writeShort(styleId)
          writeOption(stream, page)
        case SingleBeer(itemType, itemId) =>
          stream.writeShort(5)
          stream.writeShort(itemType.id)
          stream.writeShort(itemId)
        case Settings() =>
          stream.writeShort(6)
        case ChangeSorting(eitherSortingOrNothing) =>
          stream.writeShort(7)
          stream.writeBoolean(eitherSortingOrNothing.isLeft)

          if (eitherSortingOrNothing.isRight)
            stream.writeShort(
              eitherSortingOrNothing.right.get.map(_.id).getOrElse(ResetSortingValue)
            )
        case ToggleNotifications() =>
          stream.writeShort(8)
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
          val query = stream.readUTF()
          val page  = shortToOption(stream.readShort())
          Search(query, page)
        case 2 =>
          val page = shortToOption(stream.readShort())
          Menu(page)
        case 3 =>
          val page = shortToOption(stream.readShort())
          Styles(page)
        case 4 =>
          val styleId = stream.readShort()
          val page    = shortToOption(stream.readShort())
          BeersByStyle(styleId, page)
        case 5 =>
          val itemType = ItemType(stream.readShort())
          val itemId   = stream.readShort()
          SingleBeer(itemType, itemId)
        case 6 => Settings()
        case 7 =>
          val hasSorting = !stream.readBoolean()
          if (!hasSorting) {
            ChangeSorting(().asLeft)
          } else {
            val sorting = stream.readShort()
            ChangeSorting(Sorting.all.find(s => s.id == sorting).asRight)
          }
        case 8 => ToggleNotifications()
      }

      result.some
    } catch {
      case e: Throwable =>
        logger.error("Failed to deserialize callback", e)
        None
    }
  }
}
