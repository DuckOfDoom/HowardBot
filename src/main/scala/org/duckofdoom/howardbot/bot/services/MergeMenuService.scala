package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer

trait MergeMenuService {}

// TODO: Write refresh history!
class MergeMenuServiceImpl(implicit val config: Config) extends MergeMenuService {

  /**
    * Merges a new and old menu, producing new menu with correct timestamps and 'in stock' flags.
    */
  def merge(savedMenu: List[Beer], newMenu: List[Beer]): List[Beer] = {
    newMenu
  }

  /**
    * Returns items that are considered new
    */
  def getNewItems(items: List[Beer]): List[Beer] = {
    items.filter(b => Duration.between(LocalDateTime.now, b.dateAdded).toHours < config.newItemsDurationInHours)
  }
}
