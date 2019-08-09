package org.duckofdoom.howardbot.bot.data

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.MenuTab.MenuTab
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.utils.HttpService
import slogging.StrictLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait ItemDataProvider {

  /**
    * Refresh the list of items available
    */
  def refresh(): Unit

  /**
    * Get all items available
    */
  def allItems: Iterable[Item]

  /**
    * Get specific item by id
    */
  def getItem(itemId: Int): Option[Item]

  /**
    * Get items for specific menu tab
    */
  def getItems(menuTab: MenuTab): List[Item]
}

class ParsedItemsDataProvider(httpService: HttpService, config: Config)
    extends ItemDataProvider
    with StrictLogging {

  private var items: Map[Int, Item] = Map()

  override def allItems: Iterable[Item]           = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)

  // TODO: Make this async
  override def refresh(): Unit = {

    implicit val ec: ExecutionContext = ExecutionContext.global

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
        items =
          new MenuParser(mainOutput, additionalPages).parse().map(item => (item.id, item)).toMap
      case _ => logger.error("Refresh failed, got empty results!")
    }
  }

  /**
    * Get items for specific menu togwrijlgerklgertjhilgbwjiweab
    */
  override def getItems(menuTab: MenuTab): List[Item] = {
    throw new NotImplementedError("Not implemented yet")
  }

  refresh()
}

class FakeItemDataProvider extends ItemDataProvider {
  private var items: Map[Int, Item]                = Map()
  private val itemsByTab: Map[MenuTab, List[Item]] = Map()

  override def allItems: Iterable[Item]           = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)

  /**
    * Refresh the list of items available
    */
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

  }

  /**
    * Get items for specific menu tab
    */
  override def getItems(menuTab: MenuTab): List[Item] = {
    itemsByTab.get(menuTab).getOrElse(List[Item]())
  }
}
