package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.services.MenuMergeServiceImpl
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.services.HttpService
import org.duckofdoom.howardbot.utils.{FileUtils, TimeUtils}
import slogging.StrictLogging
import org.duckofdoom.howardbot.utils.Extensions._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object ItemsProvider {
  val savedMenuFilePath     = "menu.json"
  val menuChangelogFilePath = "menu_changelog.txt"
}

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
    * Get all items known. Includes beers that are out of stock.
    */
  def beers: Seq[Beer]

  /**
    * Get all items available for customers.
    * Does not include beers that are out of stock, but includes beers on deck
    */
  def beersInStock: Seq[Beer]

  /**
    * Get all styles known. Includes styles for beers that are out of stock.
    */
  def styles: Seq[Style]

  /**
    * Get all styles available for customers.
    * Does not include styles for beers that are not in stock now.
    */
  def stylesInStock: Seq[Style]

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
  def findBeersByStyle(styleName: String): Seq[Beer]

  /**
    * Get items for specific style by its id
    */
  def findBeersByStyle(styleId: Int): Seq[Beer]
}

/**
  * Base class for items provider
  */
abstract class ItemsProviderBase extends ItemsProvider with StrictLogging {

  override def lastRefreshTime: LocalDateTime     = _lastRefreshTime
  override def beers: Seq[Beer]                   = _beers
  override def beersInStock: Seq[Beer]            = _beers.filter(b => b.isInStock)
  override def styles: Seq[Style]                 = _styles
  override def stylesInStock: Seq[Style]          = _stylesInStock
  override def getBeer(itemId: Int): Option[Beer] = _beersMap.get(itemId)
  override def getStyleId(styleName: String): Option[Int] =
    _stylesMap.find { case (_, st) => st.name == styleName }.map(_._1)
  override def getStyle(id: Int): Option[Style] = _stylesMap.get(id)

  protected var _lastRefreshTime: LocalDateTime          = LocalDateTime.MIN
  protected var _beersMap: Map[Int, Beer]                = Map()
  protected var _beersByStyleMap: Map[String, Seq[Beer]] = Map()
  protected var _stylesMap: Map[Int, Style]              = Map()
  protected var _beers: Seq[Beer]                        = List()
  protected var _styles: Seq[Style]                      = List()
  protected var _stylesInStock: Seq[Style]               = List()

  /**
    * Get items for specific style
    */
  override def findBeersByStyle(style: String): Seq[Beer] = {
    if (_beersByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    _beersByStyleMap.getOrElse(
      style,
      _beersByStyleMap.keys
        .filter(_.toLowerCase.contains(style.toLowerCase))
        .foldLeft(new mutable.ListBuffer[Beer])((list, style) => list ++ _beersByStyleMap(style))
        .toList
    )
  }

  override def findBeersByStyle(styleId: Int): Seq[Beer] = {
    if (_beersByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    val mBeers = for {
      style <- _stylesMap.get(styleId)
      items <- _beersByStyleMap.get(style.name)
    } yield items

    mBeers.getOrElse(List())
  }
}

/**
  * Items provider that can refresh itself via parsing html
  */
class ParsedItemsProvider(implicit httpService: HttpService, config: Config) extends ItemsProviderBase {

  import io.circe.syntax._
  import io.circe.parser._

  val mergeService = new MenuMergeServiceImpl

  logger.info(
    s"${getClass.getName} created. Refresh period: ${config.menuRefreshPeriod} seconds. Timeout: ${config.httpRequestTimeout} seconds."
  )

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
                        )
                    )
                )
      } yield (mainOutput, pages.filter(_.isDefined).map(_.get).toList)

      val result = Try(Await.result(resultsFuture, config.httpRequestTimeout seconds)).toEither

      result match {
        case Right((Some(mainOutput), additionalPages)) =>
          logger.info(s"Got main output and ${additionalPages.length} additional pages.")

          if (mainOutput.isEmpty) {
            logger.error("Main output is empty. Skipping this refresh to no overwrite the menu.")
            return
          }

          val beersMap: mutable.Map[Int, Beer]                               = mutable.Map()
          val stylesMap: mutable.Map[Int, Style]                             = mutable.Map()
          val beersByStyleMap: mutable.Map[String, mutable.ListBuffer[Beer]] = mutable.Map()
          val stylesInStock: mutable.Set[String]                             = mutable.Set()

          var styleId = 0

          val savedMenu               = loadSavedMenu()
          val parsedMenu              = new MenuParser(mainOutput, additionalPages).parse()
          val (mergedMenu, changelog) = mergeService.merge(savedMenu.getOrElse(Seq()), parsedMenu)

          saveMenuAndChangelog(mergedMenu, changelog)

          for (item <- mergedMenu) {
            // Items without breweries are food, we ignore them for now
            if (item.breweryInfo.name.isDefined) {

              if (item.style.isDefined) {

                val style = item.style.get
                // What if we can't cover all styles with regex? What if we'll have cyrillic styles?
                if (beersByStyleMap.contains(style))
                  beersByStyleMap(style) += item
                else {
                  styleId += 1
                  stylesMap(styleId) = Style(styleId, style)
                  beersByStyleMap(style) = mutable.ListBuffer[Beer](item)
                }

                if (item.isInStock)
                  stylesInStock.add(style)

                beersMap(item.id) = item
              }

              _beersMap = beersMap.toMap
              _beers = _beersMap.values.toList
              _beersByStyleMap = beersByStyleMap.map {
                case (style: String, list: mutable.ListBuffer[Beer]) => (style, list.toList)
              }.toMap

              _stylesMap = stylesMap.toMap
              _styles = stylesMap.values.toList
              _stylesInStock = stylesMap.values.filter(st => stylesInStock.contains(st.name)).toList

              _lastRefreshTime = LocalDateTime.now
            }
          }
        case Right(_) =>
          logger.error(s"Refresh failed! Got empty results!")
        case Left(ex) =>
          logger.error(s"Refresh due to exception:$ex")
      }
    }

    Future {
      while (true) {
        refreshSync()
        Thread.sleep((config.menuRefreshPeriod * 1000).toInt)
      }
    }
  }

  private def saveMenuAndChangelog(menu: Seq[Beer], changelog: Seq[String]): Unit = {
    val menuJson = menu.asJson.toString
    FileUtils.writeFile(ItemsProvider.savedMenuFilePath, menuJson)

    val currentChangelogStr = {
      if (changelog.nonEmpty)
        s"""${TimeUtils.formatDateTime(LocalDateTime.now)}
           |  ${changelog.length} change(s):
           |${changelog.mkString("\n")}\n\n""".stripMargin.normalizeNewlines
      else
        ""
    }

    val previousChangelog = FileUtils.readFile(ItemsProvider.menuChangelogFilePath)
    val mergedChangelog   = currentChangelogStr + previousChangelog.getOrElse("")

    FileUtils.writeFile(ItemsProvider.menuChangelogFilePath, mergedChangelog)
  }

  private def loadSavedMenu(): Option[Seq[Beer]] = {
    FileUtils.readFile(ItemsProvider.savedMenuFilePath).map(decode[Seq[Beer]]) match {
      case Some(Right(beers)) => beers.some
      case _ =>
        logger.info(s"Can't decode saved menu from '${ItemsProvider.savedMenuFilePath}'.")
        None
    }
  }
}

class FakeBeersProvider extends ItemsProviderBase {

  def startRefreshLoop()(implicit ec: ExecutionContext): Future[Unit] = {

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
    Future.successful()
  }

}
