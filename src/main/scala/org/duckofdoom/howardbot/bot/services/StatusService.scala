package org.duckofdoom.howardbot.bot.services

import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.bot.BotStarter
import org.duckofdoom.howardbot.bot.data.ItemsProvider
import org.duckofdoom.howardbot.utils.TimeUtils
import scalatags.Text.all._

/**
  * Provides info about running services
  */
class StatusService(implicit bot: BotStarter, itemProvider: ItemsProvider) {

  def getStatusInfoHtml: String = {

    val restartReason = bot.restartReason match {
      case Some(r) => s"Last time I've been restarted because of this:\n$r"
      case None    => ""
    }

    html(
      head(
        ),
      body(
        h1("Hey there!"),
        div(
          p(s"I've been running for ${TimeUtils.formatDuration(bot.runningTime)}"),
          p(s"Last menu refresh: ${TimeUtils.formatDateTime(itemProvider.lastRefreshTime)} (${TimeUtils.formatDuration(
            Duration.between(itemProvider.lastRefreshTime, LocalDateTime.now))} ago)"),
          p(s"I have ${itemProvider.beers.length} items in menu."),
          p(s"I've been restarted ${bot.restartCount} times!"),
          p(restartReason)
        )
      )
    ).render
  }
}
