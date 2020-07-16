package org.duckofdoom.howardbot.bot.services

import org.duckofdoom.howardbot.bot.data.Beer

object MenuMergeService {
  final val newItemInStock  = "New item: %s"
  final val newItemOnDeck   = "New item on deck: %s"
  final val fromStockToDeck = "Item went from stock to deck: %s"
  final val fromDeckToStock = "Item went from deck to stock: %s"
  final val inStockAgain    = "Item is in stock again: %s"
  final val onDeckAgain     = "Item is on deck again: %s"
  final val outOfStock      = "Item went out of stock: %s"

  final val itemWithoutAName    = "Error: encountered %s item without a name: %s"
  final val itemWithoutALink    = "Error: encountered %s item without a link: %s"
  final val itemWithInvalidLink = "Error: encountered %s item with an invalid link: %s"
}

trait MenuMergeService {
  def merge(savedItems: Seq[Beer], newItems: Seq[Beer.ParsedInfo]): (Seq[Beer], Seq[String])
}


