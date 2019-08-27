package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.HttpService
import slogging.StrictLogging

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait ItemsProvider {

  /**
    * Time since last refresh
    */
  def lastRefreshTime: LocalDateTime

  /**
    * Refresh the list of items available
    */
  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit]

  /**
    * Get all items available
    */
  def items: List[Beer]

  /**
    * Get all styles available
    */
  def styles: List[Style]

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
    * Get items for specific style
    */
  def findBeersByStyle(styleName: String): List[Beer]

  /**
    * Get items for specific style by its id
    */
  def findBeersByStyle(styleId: Int): List[Beer]
}

/**
  * Base class for items provider
  */
abstract class ItemsProviderBase extends ItemsProvider with StrictLogging {

  override def lastRefreshTime: LocalDateTime     = _lastRefreshTime
  override def items: List[Beer]                  = _items
  override def styles: List[Style]                = _styles
  override def getBeer(itemId: Int): Option[Beer] = _itemsMap.get(itemId)
  override def getStyleId(styleName: String): Option[Int] =
    _stylesMap.find { case (_, st) => st.name == styleName }.map(_._1)
  override def getStyle(id: Int): Option[Style] = _stylesMap.get(id)

  protected var _lastRefreshTime: LocalDateTime           = LocalDateTime.MIN
  protected var _itemsMap: Map[Int, Beer]                 = Map()
  protected var _itemsByStyleMap: Map[String, List[Beer]] = Map()
  protected var _stylesMap: Map[Int, Style]               = Map()
  protected var _items: List[Beer]                        = List()
  protected var _styles: List[Style]                      = List()

  /**
    * Get items for specific style
    */
  override def findBeersByStyle(style: String): List[Beer] = {
    if (_itemsByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    _itemsByStyleMap.getOrElse(
      style,
      _itemsByStyleMap.keys
        .filter(_.toLowerCase.contains(style.toLowerCase))
        .foldLeft(new mutable.MutableList[Beer])((list, style) => list ++= _itemsByStyleMap(style))
        .toList
    )
  }

  override def findBeersByStyle(styleId: Int): List[Beer] = {
    if (_itemsByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    val mBeers = for {
      style <- _stylesMap.get(styleId)
      items <- _itemsByStyleMap.get(style.name)
    } yield items

    mBeers.getOrElse(List())
  }
}

/**
  * Beers provider that can refresh itself via parsing html
  */
class ParsedItemsProvider(implicit httpService: HttpService, config: Config)
    extends ItemsProviderBase {

  logger.info(
    s"ParsedBeersDataProvider created. Refresh period: ${config.menuRefreshPeriod} seconds.")

  override def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit] = {

    def refreshSync(): Unit = {

      logger.info("Refreshing items...")

      val resultsFuture = for {
        mainOutput <- httpService.makeRequestAsync(config.mainMenuUrl)
        pages <- Future.sequence(
          (1 to config.additionalPagesCount)
            .map(
              p =>
                httpService.makeRequestAsync(
                  config.getAdditionalResultPageUrl(p)
              ))
        )
      } yield (mainOutput, pages.filter(_.isDefined).map(_.get).toList)

      val result = Await.result(resultsFuture, Duration.Inf)

      result match {
        case (Some(mainOutput), additionalPages) =>
          logger.info(s"Got main output and ${additionalPages.length} additional pages.")

          val itemsMap: mutable.Map[Int, Beer]                                = mutable.Map()
          val stylesMap: mutable.Map[Int, Style]                              = mutable.Map()
          val itemsByStyleMap: mutable.Map[String, mutable.MutableList[Beer]] = mutable.Map()

          var styleId = 0

          for (item <- new MenuParser(mainOutput, additionalPages).parse()) {

            if (item.style.isDefined) {

              val style = item.style.get

              // What if we can't cover all styles with regex? What if we'll have cyrillic styles?
              if (itemsByStyleMap.contains(style))
                itemsByStyleMap(style) += item
              else {
                styleId += 1
                stylesMap(styleId) = Style(styleId, style)
                itemsByStyleMap(style) = mutable.MutableList[Beer](item)
              }
            }

            itemsMap(item.id) = item
          }

          _itemsMap = itemsMap.toMap
          _items = _itemsMap.values.toList
          _itemsByStyleMap = itemsByStyleMap.map {
            case (style: String, list: mutable.MutableList[Beer]) => (style, list.toList)
          }.toMap

          _stylesMap = stylesMap.toMap
          _styles = stylesMap.values.toList

          _lastRefreshTime = LocalDateTime.now
        case _ => logger.error("Refresh failed, got empty results!")
      }
    }

    Future {
      while (true) {
        refreshSync()
        Thread.sleep((config.menuRefreshPeriod * 1000).toInt)
      }
    }
  }
}

class FakeBeersProvider extends ItemsProviderBase {

  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit] = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    _itemsMap = (0 to 10)
      .map(i => {
        val item = Beer(
          i,
          i.some,
          faker.Lorem.words(2).head.capitalize.some,
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

    _items = _itemsMap.values.toList
    _lastRefreshTime = LocalDateTime.now()
    Future.successful()
  }

}
