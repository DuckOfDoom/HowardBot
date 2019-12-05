package org.duckofdoom.howardbot.services

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.db.DB
import slogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scalatags.Text.all._

/* Service that is used to send notifications for users */

class NotificationsService(implicit db: DB, bot: Bot, ec: ExecutionContext) extends StrictLogging {

  /*
   Sends notifications to users.
   Returns a list of errors for each user.
   */
  def sendNotification(title: String, message: String): Future[Seq[String]] = {
    val myId = 265958901
    val usersToSendMessages = db.users.filter { u =>
      u.userId == myId 
    }

    Future.sequence(
      usersToSendMessages.map { u =>
        {
          val strUser = f"${u.username} (${u.userId})"
          
          bot.sendNotification(u.userId, title, message).transform {
            case Success(_) =>
              val msg = f"Successfully sent notification to $strUser"
              logger.info(msg)
              Success(msg)
            case Failure(ex) =>
              val msg = f"Failed to send notification to $strUser because of error: ${ex.getMessage}"
              logger.error(msg)
              Success(msg)
          }
        }
      }
    )
  }
}
