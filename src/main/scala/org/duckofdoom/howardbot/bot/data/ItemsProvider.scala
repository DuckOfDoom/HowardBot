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
  def items: List[Item]

  /**
    * Get all styles available
    */
  def styles : List[String]

  /**
    * Get specific item by id
    */
  def getItem(itemId: Int): Option[Item]

  /**
    * Get items for specific style
    */
  def findItemsByStyle(style: String): List[Item]
}

/**
  * Base class for items provider
  */
abstract class ItemsProviderBase extends ItemsProvider with StrictLogging {

  override def lastRefreshTime: LocalDateTime     = _lastRefreshTime
  override def items: List[Item]                  = _items
  override def styles: List[String]               = _stylesMap.keys.toList
  override def getItem(itemId: Int): Option[Item] = _itemsMap.get(itemId)

  protected var _lastRefreshTime: LocalDateTime     = LocalDateTime.MIN
  protected var _itemsMap: Map[Int, Item]           = Map()
  protected var _stylesMap: Map[String, List[Item]] = Map()
  protected var _items: List[Item]                  = List()

  /**
    * Get items for specific style
    */
  override def findItemsByStyle(style: String): List[Item] = {
    if (_stylesMap.isEmpty)
      logger.error("Styles map is empty!")

    _stylesMap.keys
      .filter(_.contains(style))
      .foldLeft(new mutable.MutableList[Item])((list, style) => list ++= _stylesMap(style))
      .toList
  }
}

/**
  * Items provider that can refresh itself via parsing html
  */
class ParsedItemsProvider(implicit httpService: HttpService, config: Config) extends ItemsProviderBase {

  logger.info(
    s"ParsedItemsDataProvider created. Refresh period: ${config.menuRefreshPeriod} seconds.")

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

          val itemsMap: mutable.Map[Int, Item]                          = mutable.Map()
          val stylesMap: mutable.Map[String, mutable.MutableList[Item]] = mutable.Map()

          for (item <- new MenuParser(mainOutput, additionalPages).parse()) {
            itemsMap(item.id) = item

            if (item.style.isDefined) {
              if (stylesMap.contains(item.style.get))
                stylesMap(item.style.get) += item
              else
                stylesMap(item.style.get) = mutable.MutableList[Item](item)
            }

          }

          _itemsMap = itemsMap.toMap
          _items = _itemsMap.values.toList
          _stylesMap = stylesMap.toMap {
            case (style: String, list: mutable.MutableList[Item]) => (style, list.toList)
          }
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

/**
  * Parse
  */
class FakeItemsProvider extends ItemsProviderBase {

  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit] = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    _itemsMap = (0 to 10)
      .map(i => {
        val item = MenuItem(
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
