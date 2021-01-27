package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.{Beer, Style}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import cats.syntax.option._

/**
  * Items provider that can refresh itself via parsing html
  */
class ItemsProviderImpl() extends ItemsProviderBase {

  def fillItems(beers: Seq[Beer]): Unit = {

    val availableBeers: mutable.ListBuffer[Beer]                      = mutable.ListBuffer[Beer]()
    val beersMap: mutable.Map[Int, Beer]                              = mutable.Map()
    val stylesMap: mutable.Map[Int, Style]                            = mutable.Map()
    val beersByStyleMap: mutable.Map[Style, mutable.ListBuffer[Beer]] = mutable.Map()
    val availableStyles: mutable.Set[Style]                           = mutable.Set()
    val availableShortStyles: mutable.Set[Style]                      = mutable.Set()

    var styleId = 1 // Style ids do not have to be persistent, they are not saved anywhere (for now...)

    def addBeerWithStyle(beer: Beer, shortStyle: Boolean): Option[Style] = {
      val styleName = beer.style.getOrElse("Unknown")
      val name = if (shortStyle) {
        val dashIdx = styleName.indexOf(" - ")
        if (dashIdx >= 0)
          styleName.substring(0, dashIdx)
        else {
          // Unshortable styles (without a dash) are considered full styles and should not be duplicated
          return Option.empty[Style]
        }
      } else
        styleName

      val presentStyle = beersByStyleMap.keys.find(st => st.name == name)
      if (presentStyle.isDefined) {
        beersByStyleMap(presentStyle.get) += beer
        presentStyle
      } else {
        val newStyle = Style(styleId, name)
        stylesMap(styleId) = newStyle
        beersByStyleMap(newStyle) = mutable.ListBuffer[Beer](beer)
        styleId += 1
        newStyle.some
      }
    }

    for (beer <- beers) {
      // Items without breweries are food, we ignore them for now
      if (beer.breweryInfo.name.isDefined) {
        if (beer.style.isDefined) {
          val shortStyle = addBeerWithStyle(beer, shortStyle = true)
          val style      = addBeerWithStyle(beer, shortStyle = false)

          if (beer.isInStock && !beer.isOnDeck) {
            availableBeers.append(beer)
            style.map(st => availableStyles.add(st))
            shortStyle.map(st => availableShortStyles.add(st))
          }

          beersMap(beer.id) = beer
        }
      }
    }

    _beersMap = beersMap.toMap
    _beers = _beersMap.values.toList

    // immutable to mutable
    def mapToMap[Style, V](m: mutable.Map[Style, ListBuffer[V]]): Map[Style, List[V]] = {
      m.map { case (style: Style, list: mutable.ListBuffer[V]) => (style, list.toList) }
    }.toMap

    _beersByStyleMap = mapToMap(beersByStyleMap)
    _stylesMap = stylesMap.toMap
    _availableBeers = availableBeers.toList
    _availableStyles = availableStyles.toList
    _availableShortStyles = availableShortStyles.toList

    _lastRefreshTime = LocalDateTime.now
  }
}
