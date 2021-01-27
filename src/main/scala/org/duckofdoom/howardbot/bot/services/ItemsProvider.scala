package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.{Beer, Style}

object ItemsProvider {
  val savedMenuFilePath     = "menu.json"
  val menuChangelogFilePath = "menu_changelog.txt"
}

trait ItemsProvider {

  /**
    * Fills items
    */
  def fillItems(beers: Seq[Beer]): Unit

  /**
    * Time since last refresh
    */
  def lastRefreshTime: LocalDateTime

  /**
    * Get all items known. Includes beers that are out of stock.
    */
  def beers: Seq[Beer]

  /**
    * Get all items available for customers.
    * Includes only beers in stock and not on deck
    */
  def availableBeers: Seq[Beer]

  /**
    * Get all styles known. Includes styles for beers that are out of stock.
    */
  def styles: Seq[Style]

  /**
    * Get all styles available for customers.
    * Includes only styles of beers in stock and not on deck
    */
  def getAvailableStyles(shortened: Boolean): Seq[Style]

  /**
    * Gets id for a style
    */
  def getStyleId(styleName: String): Option[Int]

  /**
    * Gets a style by id
    */
  def getStyle(id: Int): Option[Style]

  /**
    * Get specific item by id
    */
  def getBeer(itemId: Int): Option[Beer]

  /**
    * Get items for specific style by its id
    */
  def findBeerByStyleId(styleId: Int, includeOutOfStock: Boolean = false): Seq[Beer]
}




