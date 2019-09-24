package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import slogging.StrictLogging

trait MergeMenuService {}

// TODO: Write refresh history!
class MergeMenuServiceImpl(implicit val config: Config) extends MergeMenuService with StrictLogging {

  /**
    * Merges a new and old menu, producing new menu with correct timestamps and 'in stock' flags.
    * Returns a new menu and a sequence of strings describing changes
    */
  def merge(savedMenu: Seq[Beer], newItems: Seq[Beer.ParsedInfo]): (Seq[Beer], Seq[String]) = {
    
    val now = LocalDateTime.now
    val savedItemsById = savedMenu.map(i => i.id).zip(savedMenu).toMap
    val savedItemsByName = savedMenu.map(i => i.name.getOrElse("")).zip(savedMenu).toMap
    val newItemsByName = newItems.filter(_.name.isDefined).map(_.name.get).toSet

    var result : Seq[Beer] = Seq()
    var changeLog : Seq[String] = Seq()
    
    //  Check all parsed items if the were already present
    for (pItem <- newItems){
      if (pItem.name.isEmpty) {
        changeLog :+= s"Encountered an item without name: $pItem" 
      } else {
        val n = pItem.name.get

        savedItemsByName.get(n) match {
          // Brand new item.
          case None =>
            val newId = savedItemsById.keys.max + 1
            changeLog :+= s"New item: ${pItem.name}"
            result :+= Beer.fromParsedInfo(newId, isInStock = true, now, pItem)
          // This item was already present, update info.
          case Some(item) => 
            result :+= Beer.fromAnotherBeer(isInStock = true, now, item)
        } 
      }
    }
    
    // Filter out all out-of-stock items
    for (oldItem <- 
    
    logger.info(s"Updated items. Count: ${result.length}. Changes:\n${changeLog.mkString("\n")}")
    
    (result, changeLog)
  }

  /**
    * Returns items that are considered new
    */
  def getNewItems(items: List[Beer]): List[Beer] = {
    items.filter(b => Duration.between(LocalDateTime.now, b.dateAdded).toHours < config.newItemsDurationInHours)
  }
}
