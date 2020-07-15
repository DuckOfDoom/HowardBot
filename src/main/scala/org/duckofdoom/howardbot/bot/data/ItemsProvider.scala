package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import slogging.StrictLogging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

class FakeItemsProvider extends ItemsProviderBase {

  override def fillItems(beers: Seq[Beer]): Unit = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    _beersMap = (0 to 10)
      .map(i => {
        val item = Beer(
          i,
          isInStock = true,
          LocalDateTime.now(),
          LocalDateTime.now(),
          faker.Lorem.words(2).head.capitalize.some,
          i.some,
          (scala.util.Random.nextFloat() * 5f, 5f).some,
          "http://url.com".some,
          "http://pic_url.com".some,
          (scala.util.Random.nextInt(20) / 1000f).some,
          (scala.util.Random.nextInt(100) / 1000f).some,
          BreweryInfo(
            (faker.Company.name + " Brewery").some,
            "http://brewery_url.com".some,
            faker.Address.street_address().some
          ),
          mkStyle().some,
          (200 + scala.util.Random.nextInt(300) + "ml Draft").some,
          ("\u20BD", i * 100f).some,
          faker.Lorem.paragraph().some
        )

        (i, item)
      })
      .toMap

    _beers = _beersMap.values.toList
    _lastRefreshTime = LocalDateTime.now()
  }
}
