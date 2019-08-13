package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.HttpService
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait ItemsProvider {

  /**
    * Time since last refresh
    */
  def lastRefreshTime: LocalDateTime

  /**
    * Current items count
    */
  def itemsCount: Int

  /**
    * Refresh the list of items available
    */
  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit]

  /**
    * Get all items available
    */
  def allItems: Iterable[Item]

  /**
    * Get specific item by id
    */
  def getItem(itemId: Int): Option[Item]

  /**
    * Get items for specific style
    */
  def findItemsByStyle(style: String): List[Item]
}

abstract class ItemsProviderBase extends ItemsProvider {

  override def lastRefreshTime: LocalDateTime     = _lastRefreshTime
  override def itemsCount: Int                    = _items.count(_ => true)
  override def allItems: Iterable[Item]           = _items.values
  override def getItem(itemId: Int): Option[Item] = _items.get(itemId)

  protected var _lastRefreshTime: LocalDateTime = LocalDateTime.MIN
  protected var _items: Map[Int, Item]          = Map()

  /**
    * Get items for specific style
    */
  override def findItemsByStyle(style: String): List[Item] = {
    allItems.filter(i => i.style.fold(false)(_.contains(style))).toList
  }
}

class ParsedItemsProvider(httpService: HttpService, config: Config)
    extends ItemsProviderBase
    with StrictLogging {

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
          _items = new MenuParser(mainOutput, additionalPages)
            .parse()
            .map(item => (item.id, item))
            .toMap
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

  /**
    * Refresh the list of items available
    */
  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit] = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    _items = (0 to 10)
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

    _lastRefreshTime = LocalDateTime.now()
    Future.successful()
  }

}
