package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import io.circe.parser.decode
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.parser.MenuParser
import org.duckofdoom.howardbot.services.HttpService
import org.duckofdoom.howardbot.utils.{FileUtils, TimeUtils}
import slogging.StrictLogging

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try
import cats.syntax.option._
import org.duckofdoom.howardbot.utils.Extensions._
import scala.concurrent.duration._
import io.circe.syntax._

trait MenuRefreshService {
  def startRefreshLoop(onChanged: Seq[String] => Unit)(implicit ec: ExecutionContext): Future[Unit]
}

class MenuRefreshServiceImpl(
    refreshPeriod : Float,
    mainMenuUrl: String,
    additionalPagesCount : Int,
    getAdditionalResultPageUrl: (Int => String),
    httpRequestTimeout : Int,
    itemsProvider: ItemsProvider,
    httpService: HttpService,
)(
    implicit ec: ExecutionContext
) extends MenuRefreshService
    with StrictLogging {
  
  logger.info(
    s"""Created: refreshPeriod:$refreshPeriod, mainMenuUrl:$mainMenuUrl, additionalPagesCount:$additionalPagesCount
        httpRequestTimeout:$httpRequestTimeout
        """
  )
  
  val mergeService = new MenuMergeServiceImpl()

  override def startRefreshLoop(onChanged: Seq[String] => Unit)(implicit ec: ExecutionContext): Future[Unit] = {

    def refreshSync(): Seq[String] = {

      logger.info("Refreshing items...")

      val resultsFuture = for {
        mainOutput <- httpService.makeRequestAsync(mainMenuUrl)
        pages <- Future.sequence(
                  (1 to additionalPagesCount)
                    .map(
                      p =>
                        httpService.makeRequestAsync(
                          getAdditionalResultPageUrl(p)
                        )
                    )
                )
      } yield (mainOutput, pages.filter(_.isDefined).map(_.get).toList)

      val result = Try(Await.result(resultsFuture, httpRequestTimeout seconds)).toEither

      result match {
        case Right((Some(mainOutput), additionalPages)) =>
          logger.info(s"Got main output and ${additionalPages.length} additional pages.")

          if (mainOutput.isEmpty) {
            logger.error("Main output is empty. Skipping this refresh to no overwrite the menu.")
            return Seq()
          }

          val savedMenu               = loadSavedMenu()
          val parsedMenu              = new MenuParser(mainOutput, additionalPages).parse()
          val (mergedMenu, changelog) = mergeService.merge(savedMenu.getOrElse(Seq()), parsedMenu)

          saveMenuAndChangelog(mergedMenu, changelog)

          itemsProvider.fillItems(mergedMenu)

          changelog

        case Right(_) =>
          logger.error(s"Refresh failed! Got empty results!")
          Seq()
        case Left(ex) =>
          logger.error(s"Refresh failed due to exception:$ex")
          Seq()
      }
    }

    Future {
      while (true) {
        val changelog = refreshSync()
        onChanged(changelog)
        Thread.sleep((refreshPeriod * 1000).toInt)
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
