package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import slogging.StrictLogging

object MenuMergeService {
  final val newItem          = "New item: %s"
  final val inStockAgain     = "Item is in stock again: %s"
  final val wentOutOfStock   = "Item went out of stock: %s"
  final val itemWithoutAName = "Error: encountered %s item without a name: %s"
  final val itemWithoutALink = "Error: encountered %s item without a link: %s"
  final val itemWithInvalidLink = "Error: encountered %s item with an invalid link a name: %s"
}

trait MenuMergeService {
  def merge(savedItems: Seq[Beer], newItems: Seq[Beer.ParsedInfo]): (Seq[Beer], Seq[String])
}

class MenuMergeServiceImpl(val timeProvider: () => LocalDateTime = LocalDateTime.now)(implicit val config: Config)
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
    
    val now              = timeProvider()
    val savedItemsById   = savedItems.map(i => i.id).zip(savedItems).toMap
    val savedItemsByLinkId = savedItems.map(i => getItemEqualityId(i.link))
      .zip(savedItems)
      .filter(_._1.isDefined)
      .map(t => (t._1.get, t._2))
      .toMap
    
    val newItemsLinkIds    = newItems.flatMap(i => getItemEqualityId(i.link)).toSet

    var newId = if (savedItemsById.nonEmpty) savedItemsById.keys.max + 1 else 1

    var result: Seq[Beer]      = Seq()
    var changeLog: Seq[String] = Seq()

    def addToChangelog(msg: String, error: Boolean = false): Unit = {
      changeLog :+= msg
      if (error) logger.error(msg) else logger.info(msg)
    }

    //  Check all parsed items if the were already present
    for (nItem <- newItems) {
      getItemEqualityId(nItem.link) match {
        case None =>
          addToChangelog(itemWithInvalidLink.format("a new", nItem), error = true)
        case Some(linkId) =>
          savedItemsByLinkId.get(linkId) match {
            // Brand new item
            case None =>
              if (nItem.name.isEmpty) {
                addToChangelog(itemWithoutAName.format("a new", nItem), error = true)
              }

              addToChangelog(newItem.format(nItem.name.getOrElse("UNDEFINED")))
              result :+= Beer.fromParsedInfo(newId, isInStock = true, now, now, nItem)
              newId += 1

            // This item was already present, update info
            case Some(beer) =>
              if (nItem.name.isEmpty) {
                addToChangelog(itemWithoutAName.format("a new", nItem), error = true)
              }

              if (!beer.isInStock) {
                addToChangelog(inStockAgain.format(nItem.name.getOrElse("UNDEFINED")))
                result :+= Beer.fromAnotherBeerWithUpdatedTime(isInStock = true, now, beer)
              } else {
                result :+= Beer.fromAnotherBeer(isInStock = true, beer)
              }
          }
      }
    }

    // Filter out all out-of-stock items
    for (sItem <- savedItems) {
        getItemEqualityId(sItem.link) match {
          case None =>
            addToChangelog(itemWithInvalidLink.format("saved", sItem), error = true)
          case Some(linkId) =>
            if (sItem.name.isEmpty){
              addToChangelog(itemWithoutAName.format("saved", sItem))
            }
            
            // Went out of stock
            if (!newItemsLinkIds.contains(linkId)) {
              if (sItem.isInStock) {
                addToChangelog(wentOutOfStock.format(sItem.name.getOrElse("UNDEFINED")))
                result :+= Beer.fromAnotherBeerWithUpdatedTime(isInStock = false, now, sItem)
              } else {
                result :+= Beer.fromAnotherBeer(isInStock = false, sItem)
              }
            }
        }
      }

    logger.info(s"Updated items. Count: ${result.length}. Changes: ${changeLog.length}")

    (result, changeLog)
  }

  /**
    * Returns items that are considered new
    */
  def getNewItems(items: Seq[Beer]): Seq[Beer] = {
    items.filter(b => Duration.between(LocalDateTime.now, b.dateAdded).toHours < config.newItemsDurationInHours)
  }
}
