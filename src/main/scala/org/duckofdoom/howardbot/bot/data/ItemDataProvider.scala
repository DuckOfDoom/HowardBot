package org.duckofdoom.howardbot.bot.data

import cats.syntax.option._
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.HttpService
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

trait ItemDataProvider {
  def refresh(): Unit
  def allItems: Iterable[Item]
  def getItem(itemId: Int): Option[Item]
}

class ParsedItemsDataProvider(httpService: HttpService, parseUrl: String)
    extends ItemDataProvider
    with StrictLogging {
  private var items: Map[Int, Item] = Map()

  override def allItems: Iterable[Item]           = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)

  override def refresh(): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val result = Await.result(httpService.makeRequestAsync(parseUrl), Duration.Inf)
    result match {
      case Left(error) =>
        logger.error(s"Refresh failed. Failed to fetch menu from '$parseUrl'. Error:\n$error")
      case Right(r) =>
        items = MenuParser.parseMenu(r).map(item => (item.id, item)).toMap
    }
  }
}

class FakeItemDataProvider extends ItemDataProvider {
  private var items: Map[Int, Item] = Map()

  override def allItems: Iterable[Item]           = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)

  def apply(itemId: Int): Option[Item] = getItem(itemId)

  def refresh(): Unit = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    items = (0 to 10)
      .map(i => {
        val item = MenuItem(
          i,
          i.some,
          faker.Lorem.words(2).head.capitalize.some,
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
  }
}
