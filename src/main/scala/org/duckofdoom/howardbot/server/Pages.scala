package org.duckofdoom.howardbot.server

import java.time.Duration

import scalatags.Text.all._

object Pages {

  def homePage(implicit botRunningTime: Duration, botRestartCount: Int, lastRestartReason:Option[String]): String = {
    
    def formatTime(t:Duration) : String = {
      val d = t.toDaysPart
      val h = t.toHoursPart
      val m = t.toMinutesPart
      val s = t.toSecondsPart
      
      s"$d Days, $h Hours, $m Minutes, $s Seconds"
    }
    
    val restartReason =  lastRestartReason match {
      case Some(r) => s"Last time I've been restarted because of this:\n$r"
      case None => ""
    }
    
    html(
      head(
      ),
      body(
        h1("Hey there!"),
        div(
          p(s"I've been running for ${formatTime(botRunningTime)}"),
          p(s"I've been restarted $botRestartCount times!"),
          p(restartReason)
        )
      )
    ).render
  }

}
