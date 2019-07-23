package org.duckofdoom.howardbot.server

import java.time.Duration

import org.duckofdoom.howardbot.bot.{BotStatus, ResponseService}
import scalatags.Text.all._

trait ServerResponseService {
  def home(): String
  def menu(): String
  def show(itemId: Int): String
}

class ServerResponseServiceImpl(implicit botStatus:BotStatus, responseService: ResponseService) extends ServerResponseService {

  override def home(): String = {
    
    def formatTime(t:Duration) : String = {
      val d = t.toDaysPart
      val h = t.toHoursPart
      val m = t.toMinutesPart
      val s = t.toSecondsPart
      
      s"$d Days, $h Hours, $m Minutes, $s Seconds"
    }
    
    val restartReason = botStatus.restartReason match {
      case Some(r) => s"Last time I've been restarted because of this:\n$r"
      case None => ""
    }
    
    html(
      head(
      ),
      body(
        h1("Hey there!"),
        div(
          p(s"I've been running for ${formatTime(botStatus.runningTime)}"),
          p(s"I've been restarted ${botStatus.restartCount} times!"),
          p(restartReason)
        )
      )
    ).render
  }
  
  override def menu() : String = { 
    responseService.mkMenuResponse("/")
  }
  
  override def show(itemId: Int) : String = { 
    responseService.mkItemResponse(itemId)
  }

}