package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.{Beer, Style}
import slogging.StrictLogging

/**
  * Base class for items provider
  */
abstract class ItemsProviderBase extends ItemsProvider with StrictLogging {

  override def lastRefreshTime: LocalDateTime     = _lastRefreshTime
  override def beers: Seq[Beer]                   = _beers
  override def availableBeers: Seq[Beer]          = _availableBeers
  override def styles: Seq[Style]                 = _stylesMap.values.toList
  override def getBeer(itemId: Int): Option[Beer] = _beersMap.get(itemId)
  override def getStyleId(styleName: String): Option[Int] =
    _stylesMap.find { case (_, st) => st.name == styleName }.map(_._1)
  override def getStyle(id: Int): Option[Style] = _stylesMap.get(id)

  protected var _lastRefreshTime: LocalDateTime         = LocalDateTime.MIN
  protected var _beersMap: Map[Int, Beer]               = Map()
  protected var _beersByStyleMap: Map[Style, Seq[Beer]] = Map()
  protected var _stylesMap: Map[Int, Style]             = Map()
  protected var _beers: Seq[Beer]                       = List()

  protected var _availableBeers: Seq[Beer]        = List()
  protected var _availableStyles: Seq[Style]      = List()
  protected var _availableShortStyles: Seq[Style] = List()

  override def getAvailableStyles(shortened: Boolean): Seq[Style] = {
    if (shortened) _availableShortStyles else _availableStyles
  }

  override def findBeerByStyleId(styleId: Int, includeOutOfStock: Boolean = false): Seq[Beer] = {
    if (_beersByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    val mBeers = for {
      style <- _stylesMap.get(styleId)
      items <- _beersByStyleMap.get(style)
    } yield items

    mBeers
      .map(_.filter(b => includeOutOfStock || (b.isInStock && !b.isOnDeck)))
      .getOrElse(List())
  }
}
