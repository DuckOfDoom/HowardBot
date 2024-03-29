package org.duckofdoom.howardbot

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.duckofdoom.howardbot.utils.FileUtils
import slogging.StrictLogging

object Config extends StrictLogging {
  val configPath = "config.json"

  def load(implicit path: String = configPath): Option[Config] =
    FileUtils
      .readFile(path)
      .flatMap(f = s => {
        decode[Config](s) match {
          case Right(c)  => Some(c)
          case Left(err) => logger.error(s"Failed to parse '$configPath'! Error: $err"); None
        }
      })

  def save(botConfig: Config, path: String = configPath): Unit = {
    FileUtils.writeFile(path, botConfig.asJson.toString)
  }
}

case class Config(
    parallelismLevel: Int,
    startBot: Boolean,
    startServer: Boolean,
    httpRequestTimeout: Int,
    token: String,
    serverAddress: String,
    serverPort: Int,
    mainMenuUrl: String,
    additionalPagesUrl: String,
    additionalPagesCount: Int,
    menuRefreshPeriod: Float,
    menuItemsPerPage: Int,
    stylesPerPage: Int,
    newItemsDurationInHours: Int,
    testNotificationsUserIds: List[Int],
    menuUpdatesNotificationsUserIds: List[Int],
    postgres: PostgresConfig
) extends StrictLogging {

  private val pageSubst = "__PAGE__"

  def getAdditionalResultPageUrl(page: Int): String = {

    if (!additionalPagesUrl.contains(pageSubst))
      logger.error(s"Invalid additionalPagesUrl format: $additionalPagesUrl")

    additionalPagesUrl.replace(pageSubst, page.toString)
  }
}

case class PostgresConfig(
    database: String,
    user: String,
    password: String
)
