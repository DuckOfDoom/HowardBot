package org.duckofdoom.howardbot.bot

import java.time
import java.time.{Duration, LocalDateTime}
import scalatags.Text.all._

import org.duckofdoom.howardbot.bot.data.ItemDataProvider

class StatusInfoProvider(implicit bot: BotStarter, itemProvider: ItemDataProvider) {
  
  def getStatusInfoHtml : String = {

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
          p(s"Last menu refresh: ${formatDate(itemProvider.lastRefreshTime)} (${formatDuration(Duration.between(itemProvider.lastRefreshTime, LocalDateTime.now))} ago)"),
          p(s"I have ${itemProvider.itemsCount} items in menu."),
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
    val year = t.getYear
    val month = t.getMonthValue
    val day = t.getDayOfMonth
    val h = t.getHour
    val m = t.getMinute
    val s = t.getSecond
    
    time.Duration.between(LocalDateTime.now, LocalDateTime.now)

    s"${t.toLocalTime.formatted("HH:mm:ss")} $day-$month-$year"
  }

}
