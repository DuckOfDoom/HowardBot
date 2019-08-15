package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.{CallbackUtils, Consts}
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.{HttpService, PaginationUtils}
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
  def styles: List[String]

  /**
    * Gets id for a style
    */
  def getStyleId(style: String): Option[Int]

  /**
    * Gets a style by id
    */
  def getStyle(id: Int): Option[String]

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
  override def styles: List[String]               = _styles
  override def getItem(itemId: Int): Option[Item] = _itemsMap.get(itemId)
  override def getStyleId(style: String): Option[Int] =
    _stylesMap.find { case (_, st) => st == style }.map(_._1)
  override def getStyle(id: Int): Option[String] = _stylesMap.get(id)

  protected var _lastRefreshTime: LocalDateTime           = LocalDateTime.MIN
  protected var _itemsMap: Map[Int, Item]                 = Map()
  protected var _itemsByStyleMap: Map[String, List[Item]] = Map()
  protected var _stylesMap: Map[Int, String]              = Map()
  protected var _items: List[Item]                        = List()
  protected var _styles: List[String]                     = List()

  /**
    * Get items for specific style
    */
  override def findItemsByStyle(style: String): List[Item] = {
    if (_itemsByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    _itemsByStyleMap.getOrElse(
      style,
      _itemsByStyleMap.keys
        .filter(_.toLowerCase.contains(style.toLowerCase))
        .foldLeft(new mutable.MutableList[Item])((list, style) => list ++= _itemsByStyleMap(style))
        .toList
    )
  }
}

/**
  * Items provider that can refresh itself via parsing html
  */
class ParsedItemsProvider(implicit httpService: HttpService, config: Config)
    extends ItemsProviderBase {

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

          val itemsMap: mutable.Map[Int, Item]                                = mutable.Map()
          val stylesMap: mutable.Map[Int, String]                             = mutable.Map()
          val itemsByStyleMap: mutable.Map[String, mutable.MutableList[Item]] = mutable.Map()

          var styleId = 0

          for (item <- new MenuParser(mainOutput, additionalPages).parse()) {

            if (item.style.isDefined) {

              val style = item.style.get

              // What if we can't cover all styles with regex? What if we'll have cyrillic styles?
              if (!style.matches(CallbackUtils.styleValidationRegex.regex)) {
                logger.error(
                  s"Style '$style' does not match style regex (${CallbackUtils.styleValidationRegex.regex}. Callback query will be broken!")
              } else {
                if (itemsByStyleMap.contains(item.style.get))
                  itemsByStyleMap(item.style.get) += item
                else {
                  styleId += 1
                  stylesMap(styleId) = item.style.get
                  itemsByStyleMap(item.style.get) = mutable.MutableList[Item](item)
                }
              }
            }

            itemsMap(item.id) = item
          }

          _itemsMap = itemsMap.toMap
          _items = _itemsMap.values.toList
          _itemsByStyleMap = itemsByStyleMap.map {
            case (style: String, list: mutable.MutableList[Item]) => (style, list.toList)
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
