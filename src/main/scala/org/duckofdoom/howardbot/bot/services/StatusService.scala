package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.bot.data.ItemsProvider
import org.duckofdoom.howardbot.utils.TimeUtils
import scalatags.Text.all._

/**
  * Provides info about running services
  */
class StatusService(bot: Bot, itemsProvider: ItemsProvider) {

  def getStatusInfoHtml: String = {

    val runningTimeFormatted = TimeUtils.formatDuration(bot.runningTime)
    val lastRefreshTime = TimeUtils.formatDateTime(itemsProvider.lastRefreshTime)
    val lastRefreshTimeAgo = TimeUtils.formatDuration(Duration.between(itemsProvider.lastRefreshTime, LocalDateTime.now))

    val beersCount = itemsProvider.beers.length
    val beersInStockCount = itemsProvider.availableBeers.length
    val stylesCount = itemsProvider.styles.length
    val stylesInStockCount = itemsProvider.getAvailableStyles(false).length

    html(
      head(
        ),
      body(
        h1("Hey there!"),
        div(
          p(s"I've been running for $runningTimeFormatted"),
          p(s"Last menu refresh: $lastRefreshTime ($lastRefreshTimeAgo ago)"),
          p(s"I know about $beersCount beers. $beersInStockCount of them are in stock now."),
          p(s"I know about $stylesCount different styles. $stylesInStockCount of them are in stock now."),
        )
      )
    ).render
  }
}
