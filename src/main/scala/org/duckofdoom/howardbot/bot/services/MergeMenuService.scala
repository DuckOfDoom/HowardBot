package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import slogging.StrictLogging

// TODO: TESTS
trait MergeMenuService {}

class MergeMenuServiceImpl(implicit val config: Config) extends MergeMenuService with StrictLogging {

  final val newItem          = "New item: %s"
  final val inStockAgain     = "Item is in stock again: %s"
  final val wentOutOfStock   = "Item went out of stock: %s"
  final val itemWithoutAName = "Error: encountered %s item without a name: %s"

  /**
    * Merges a new and old menu, producing new menu with correct timestamps and 'in stock' flags.
    * Returns a new menu and a sequence of strings describing changes
    */
  def merge(savedItems: Seq[Beer], newItems: Seq[Beer.ParsedInfo]): (Seq[Beer], Seq[String]) = {

    val now              = LocalDateTime.now
    val savedItemsById   = savedItems.map(i => i.id).zip(savedItems).toMap
    val savedItemsByName = savedItems.map(i => i.name.getOrElse("")).zip(savedItems).toMap
    val newItemsNames    = newItems.filter(_.name.isDefined).map(_.name.get).toSet

    var newId = if (savedItemsById.nonEmpty) savedItemsById.keys.max + 1 else 1

    var result: Seq[Beer]      = Seq()
    var changeLog: Seq[String] = Seq()

    def addToChangelog(msg: String, error: Boolean = false): Unit = {
      changeLog :+= msg
      if (error) logger.error(msg) else logger.info(msg)
    }

    //  Check all parsed items if the were already present
    for (pItem <- newItems) {
      if (pItem.name.isEmpty) {
        addToChangelog(itemWithoutAName.format("a new", pItem), error = true)
      } else {
        val itemName = pItem.name.get
        savedItemsByName.get(itemName) match {
          // Brand new item
          case None =>
            addToChangelog(newItem.format(itemName))
            result :+= Beer.fromParsedInfo(newId, isInStock = true, now, now, pItem)
            newId += 1

          // This item was already present, update info
          case Some(beer) =>
            if (!beer.isInStock) {
              addToChangelog(inStockAgain.format(itemName))
              result :+= Beer.fromAnotherBeerWithUpdatedTime(isInStock = true, now, beer)
            } else {
              result :+= Beer.fromAnotherBeer(isInStock = true, beer)
            }
        }
      }
    }

    // Filter out all out-of-stock items
    for (sItem <- savedItems) {
      if (sItem.name.isEmpty) {
        addToChangelog(itemWithoutAName.format("a new", sItem), error = true)
      } else {
        val itemName = sItem.name.get
        // Went out of stock
        if (!newItemsNames.contains(itemName) && sItem.isInStock) {
          addToChangelog(wentOutOfStock.format(itemName))
          result :+= Beer.fromAnotherBeerWithUpdatedTime(isInStock = false, now, sItem)
        } 
      }
    }

    logger.info(s"Updated items. Count: ${result.length}. Changes: ${changeLog.length}")

    (result, changeLog)
  }

  /**
    * Returns items that are considered new
    */
  def getNewItems(items: List[Beer]): List[Beer] = {
    items.filter(b => Duration.between(LocalDateTime.now, b.dateAdded).toHours < config.newItemsDurationInHours)
  }
}
