package org.duckofdoom.howardbot.server

import java.time.Duration

import org.duckofdoom.howardbot.bot.{BotStatus, ResponseService}
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User
import scalatags.Text.all._

trait ServerResponseService {
  def home(): String
  def menu(): String
  def getUsers(): String
  def getUser(userId: Int): String
  def putRandomUser(): String
  def show(itemId: Int): String
}

class ServerResponseServiceImpl(implicit botStatus:BotStatus, responseService: ResponseService, db:DB) extends ServerResponseService {

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

  override def putRandomUser(): String = {
    db.putUser(
      scala.util.Random.nextInt(Int.MaxValue),
      faker.Name.first_name,
      faker.Name.first_name,
      faker.Name.last_name
    ).toString
  }
  
  override def getUsers() : String = {
    "Users:<br>" + 
    db.users.foldLeft("")((s, u) => {
      s + (u.toString + "<br>")
    })
  }

  override def getUser(userId: Int): String = {
    db.getUser(userId).toString
  }

}
