package org.duckofdoom.howardbot.bot

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import cats.syntax.option._

import scala.util.Try

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

  final case class Menu(page: Option[Int], newMessage: Boolean)   extends Callback
  final case class Styles(page: Option[Int], newMessage: Boolean) extends Callback
  final case class ItemsByStyle(styleId: Int, page: Int)          extends Callback

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
      }

      byteArrayInputStream.toByteArray
    }
  }

  def deserialize[A <: Callback](bytes: Array[Byte]): Option[Callback] = {
    def shortToOption(i: Int) = {
      if (i >= 0) i.asInstanceOf[Int].some else None
    }

    Try({
      val stream = new DataInputStream(new ByteArrayInputStream(bytes))
      stream.readShort() match {
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
      }
    }).toOption
  }
}
