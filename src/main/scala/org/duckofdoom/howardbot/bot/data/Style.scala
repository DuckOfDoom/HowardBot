package org.duckofdoom.howardbot.bot.data

import org.duckofdoom.howardbot.bot.data.ItemType.ItemType

object Style {
  implicit def ordering[A <: Style]: Ordering[A] = Ordering.by(_.name)
}

case class Style(id: Int, name: String) extends Item {
  override val itemType: ItemType = ItemType.Style
}