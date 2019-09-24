package org.duckofdoom.howardbot.utils

import java.time.{Duration, LocalDateTime}
import java.time.format.DateTimeFormatter

object TimeUtils {

  def formatDuration(t: Duration): String = {
    val d = t.toDaysPart
    val h = t.toHoursPart
    val m = t.toMinutesPart
    val s = t.toSecondsPart

    s"$d Days, $h Hours, $m Minutes, $s Seconds"
  }

  def formatDateTime(t: LocalDateTime): String = {
    t.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))
  }
}
