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
    val Menu: Type.Value              = Value("Menu")
    val Styles: Type.Value            = Value("Styles")
    val ItemsByStyle: Type.Value      = Value("ItemsByStyle")
    val SingleBeer: Type.Value        = Value("SingleBeer")
    val SearchBeerByName: Type.Value  = Value("SearchBeerByName")
    val SearchBeerByStyle: Type.Value = Value("SearchBeerByStyle")
    val ChangeSorting: Type.Value     = Value("ChangeSorting")
  }

  final case class Menu(page: Option[Int], newMessage: Boolean)   extends Callback
  final case class Styles(page: Option[Int], newMessage: Boolean) extends Callback
  final case class ItemsByStyle(styleId: Int, page: Int)          extends Callback
  final case class SingleBeer(itemType: ItemType, itemId: Int)    extends Callback
  final case class SearchBeerByName(query: String, page: Int)     extends Callback
  final case class SearchBeerByStyle(query: String, page: Int)    extends Callback

  // TODO: Either[Unit, Option[Sorting]] is not a good type. Need to rewrite this using ADT.
  final case class ChangeSorting(sorting: Either[Unit, Option[Sorting]]) extends Callback

  private final val ResetSortingValue = -1

  def mkMenuCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Menu(page, newMessage))
  }

  def mkStylesCallbackData(page: Option[Int], newMessage: Boolean): String = {
    serializeCallback(Callback.Styles(page, newMessage))
  }

  def mkItemsByStyleCallbackData(styleId: Int, page: Int): String = {
    serializeCallback(Callback.ItemsByStyle(styleId, page))
  }

  def mkChangeSortingCallback(sorting: Either[Unit, Option[Sorting]]): String = {
    serializeCallback(Callback.ChangeSorting(sorting))
  }

  def mkItemCallback[A <: Item](item: A): Option[String] = {
    val itemType = item match {
      case _: Beer  => ItemType.Beer
      case _: Style => ItemType.Style
      case _ =>
        logger.error(s"Unknown type for item $item")
        return None
    }

    serializeCallback(Callback.SingleBeer(itemType, item.id)).some
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
      .map(_.asInstanceOf[Char])
      .mkString
  }

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
        case SingleBeer(itemType, itemId) =>
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
        case ChangeSorting(eitherSortingOrNothing) =>
          stream.writeShort(7)
          stream.writeBoolean(eitherSortingOrNothing.isLeft)

          if (eitherSortingOrNothing.isRight)
            stream.writeShort(
              eitherSortingOrNothing.right.get.map(_.id).getOrElse(ResetSortingValue)
            )
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
          SingleBeer(itemType, itemId)
        case 5 =>
          val query = stream.readUTF()
          val page  = stream.readShort()
          SearchBeerByName(query, page)
        case 6 =>
          val query = stream.readUTF()
          val page  = stream.readShort()
          SearchBeerByStyle(query, page)
        case 7 =>
          val hasSorting = !stream.readBoolean()
          if (!hasSorting) {
            ChangeSorting(().asLeft)
          } else {
            val sorting = stream.readShort()
            ChangeSorting(Sorting.all.find(s => s.id == sorting).asRight)
          }
      }

      result.some
    } catch {
      case e: Throwable =>
        logger.error("Failed to deserialize callback", e)
        None
    }
  }
}
