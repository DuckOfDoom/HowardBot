package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.Beer
import slogging.StrictLogging

class MenuMergeServiceImpl(timeProvider: () => LocalDateTime = LocalDateTime.now)
    extends MenuMergeService
    with StrictLogging {

  private val compareLinkRegex = """\/(\d+)$""".r

  /**
    * Merges a new and old menu, producing new menu with correct timestamps and 'in stock' flags.
    * Returns a new menu and a sequence of strings describing changes
    */
  def merge(savedItems: Seq[Beer], newItems: Seq[Beer.ParsedInfo]): (Seq[Beer], Seq[String]) = {

    import MenuMergeService._

    def getItemEqualityId(link: Option[String]) = {
      link.flatMap(l => compareLinkRegex.findFirstMatchIn(l).map(r => r.group(1)))
    }

    val now            = timeProvider()
    val savedItemsById = savedItems.map(i => i.id).zip(savedItems).toMap
    val savedItemsByLinkId = savedItems
      .map(i => getItemEqualityId(i.link))
      .zip(savedItems)
      .filter(_._1.isDefined)
      .map(t => (t._1.get, t._2))
      .toMap

    val newItemsLinkIds = newItems.flatMap(i => getItemEqualityId(i.link)).toSet

    var newId = if (savedItemsById.nonEmpty) savedItemsById.keys.max + 1 else 1

    var result: Seq[Beer]      = Seq()
    var changeLog: Seq[String] = Seq()

    def addToChangelog(msg: String, error: Boolean = false): Unit = {
      changeLog :+= msg
      if (error) logger.error(msg) else logger.info(msg)
    }

    //  Check all parsed items if the were already present
    for (newItem <- newItems) {
      getItemEqualityId(newItem.link) match {
        case None =>
          addToChangelog(itemWithInvalidLink.format("a new", newItem), error = true)
        case Some(linkId) =>
          savedItemsByLinkId.get(linkId) match {
            // Brand new item
            case None =>
              if (newItem.name.isEmpty) {
                addToChangelog(itemWithoutAName.format("a new", newItem), error = true)
              }

              if (newItem.isOnDeck) {
                addToChangelog(newItemOnDeck.format(newItem.name.getOrElse("UNDEFINED")))
              } else {
                addToChangelog(newItemInStock.format(newItem.name.getOrElse("UNDEFINED")))
              }

              result :+= Beer.fromParsedInfo(newId, isInStock = true, now, now, newItem)
              newId += 1

            // This item was already present, update info
            case Some(oldItem) =>
              if (newItem.name.isEmpty) {
                addToChangelog(itemWithoutAName.format("a new", newItem), error = true)
              }

              val changedOnDeckState = oldItem.isOnDeck != newItem.isOnDeck
              
              if (!oldItem.isInStock) {
                // Wasn't in stock - now in stock
                if (newItem.isOnDeck) {
                  addToChangelog(onDeckAgain.format(newItem.name.getOrElse("UNDEFINED")))
                } else {
                  addToChangelog(inStockAgain.format(newItem.name.getOrElse("UNDEFINED")))
                }

                result :+= Beer.fromAnotherBeerWithUpdatedInfo(isInStock = true, now, oldItem, newItem)
              } else {
                if (changedOnDeckState) {
                  // Was in stock, but went on/from deck
                  if (newItem.isOnDeck) {
                    addToChangelog(fromStockToDeck.format(newItem.name.getOrElse("UNDEFINED")))
                  } else {
                    addToChangelog(fromDeckToStock.format(newItem.name.getOrElse("UNDEFINED")))
                  }

                  result :+= Beer.fromAnotherBeerWithUpdatedInfo(isInStock = true, now, oldItem, newItem)
                } else {
                  // Nothing happened, but we refresh data in case something like price changes.
                  result :+= Beer.fromAnotherBeerWithUpdatedInfo(isInStock = true, oldItem.dateUpdated, oldItem, newItem)
                }
              }
          }
      }
    }

    // Filter out all out-of-stock items
    for (savedItem <- savedItems) {
      getItemEqualityId(savedItem.link) match {
        case None =>
          addToChangelog(itemWithInvalidLink.format("saved", savedItem), error = true)
        case Some(linkId) =>
          if (savedItem.name.isEmpty) {
            addToChangelog(itemWithoutAName.format("saved", savedItem))
          }

          // Went out of stock
          if (!newItemsLinkIds.contains(linkId)) {
            if (savedItem.isInStock) {
              addToChangelog(outOfStock.format(savedItem.name.getOrElse("UNDEFINED")))
              result :+= Beer.fromAnotherBeerWithUpdatedTime(isInStock = false, now, savedItem)
            } else {
              result :+= Beer.fromAnotherBeer(isInStock = false, savedItem)
            }
          }
      }
    }

    logger.info(s"Updated items. Count: ${result.length}. Changes: ${changeLog.length}")

    (result, changeLog)
  }
}
