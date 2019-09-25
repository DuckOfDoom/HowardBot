package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.services.MergeMenuServiceImpl
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.services.HttpService
import org.duckofdoom.howardbot.utils.FileUtils
import slogging.StrictLogging

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
    * Get all items available
    */
  // TODO: List to Seqs
  def beers: List[Beer]

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
  override def beers: List[Beer]                  = _beers.filter(b => b.isInStock)
  override def styles: List[Style]                = _styles
  override def getBeer(itemId: Int): Option[Beer] = _beersMap.get(itemId)
  override def getStyleId(styleName: String): Option[Int] =
    _stylesMap.find { case (_, st) => st.name == styleName }.map(_._1)
  override def getStyle(id: Int): Option[Style] = _stylesMap.get(id)

  protected var _lastRefreshTime: LocalDateTime           = LocalDateTime.MIN
  protected var _beersMap: Map[Int, Beer]                 = Map()
  protected var _beersByStyleMap: Map[String, List[Beer]] = Map()
  protected var _stylesMap: Map[Int, Style]               = Map()
  protected var _beers: List[Beer]                        = List()
  protected var _styles: List[Style]                      = List()

  /**
    * Get items for specific style
    */
  override def findBeersByStyle(style: String): List[Beer] = {
    if (_beersByStyleMap.isEmpty) {
      logger.error("Styles map is empty!")
      return List()
    }

    _beersByStyleMap.getOrElse(
      style,
      _beersByStyleMap.keys
        .filter(_.toLowerCase.contains(style.toLowerCase))
        .foldLeft(new mutable.MutableList[Beer])((list, style) => list ++= _beersByStyleMap(style))
        .toList
    )
  }

  override def findBeersByStyle(styleId: Int): List[Beer] = {
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

  val mergeService = new MergeMenuServiceImpl

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

          val beersMap: mutable.Map[Int, Beer]                                = mutable.Map()
          val stylesMap: mutable.Map[Int, Style]                              = mutable.Map()
          val beersByStyleMap: mutable.Map[String, mutable.MutableList[Beer]] = mutable.Map()

          var styleId = 0

          val savedMenu               = loadSavedMenu()
          val parsedMenu              = new MenuParser(mainOutput, additionalPages).parse()
          val (mergedMenu, changelog) = mergeService.merge(savedMenu.getOrElse(Seq()), parsedMenu)

          saveMenuAndChangelog(mergedMenu, changelog)

          for (item <- mergedMenu) {
            // Items without breweries are food, we ignore them for now
            if (item.breweryInfo.name.isDefined) {

              if (item.style.isDefined && item.isInStock) {

                val style = item.style.get
                // What if we can't cover all styles with regex? What if we'll have cyrillic styles?
                if (beersByStyleMap.contains(style))
                  beersByStyleMap(style) += item
                else {
                  styleId += 1
                  stylesMap(styleId) = Style(styleId, style)
                  beersByStyleMap(style) = mutable.MutableList[Beer](item)
                }

                beersMap(item.id) = item
              }

              _beersMap = beersMap.toMap
              _beers = _beersMap.values.toList
              _beersByStyleMap = beersByStyleMap.map {
                case (style: String, list: mutable.MutableList[Beer]) => (style, list.toList)
              }.toMap

              _stylesMap = stylesMap.toMap
              _styles = stylesMap.values.toList

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
        LocalDateTime.now + ":\n" + changelog.mkString("\n") + "\n\n"
      else 
        ""
    }
    
    val previousChangelog       = FileUtils.readFile(ItemsProvider.menuChangelogFilePath)
    var mergedChangelog: String = ""
    if (previousChangelog.isDefined) {
      mergedChangelog = currentChangelogStr + previousChangelog
    } else {
      mergedChangelog = currentChangelogStr
    }

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
