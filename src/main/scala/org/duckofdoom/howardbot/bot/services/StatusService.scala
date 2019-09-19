package org.duckofdoom.howardbot.bot.services

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}

import org.duckofdoom.howardbot.bot.BotStarter
import org.duckofdoom.howardbot.bot.data.ItemsProvider
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
          p(s"I've been running for ${formatDuration(bot.runningTime)}"),
          p(s"Last menu refresh: ${formatDate(itemProvider.lastRefreshTime)} (${formatDuration(
            Duration.between(itemProvider.lastRefreshTime, LocalDateTime.now))} ago)"),
          p(s"I have ${itemProvider.beers.length} items in menu."),
          p(s"I've been restarted ${bot.restartCount} times!"),
          p(restartReason)
        )
      )
    ).render
  }

  private def formatDuration(t: Duration): String = {
    val d = t.toDaysPart
    val h = t.toHoursPart
    val m = t.toMinutesPart
    val s = t.toSecondsPart

    s"$d Days, $h Hours, $m Minutes, $s Seconds"
  }

  private def formatDate(t: LocalDateTime): String = {
    t.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
  }
}
