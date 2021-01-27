package org.duckofdoom.howardbot.bot.data

import org.duckofdoom.howardbot.bot.data.ItemType.ItemType

trait Item {
  val id: Int
  val itemType: ItemType
}
