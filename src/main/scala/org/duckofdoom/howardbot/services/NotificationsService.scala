package org.duckofdoom.howardbot.services

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.db.DB
import slogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/* Service that is used to send notifications for users */

class NotificationsService(implicit config:Config, db: DB, bot: Bot, ec: ExecutionContext) extends StrictLogging {

  /*
   Sends notifications to users.
   Returns a list of errors for each user.
   */
  def sendNotification(title: String, message: String, isLive: Boolean): Future[Seq[String]] = {
    
    val usersToSendMessages = db.users.filter { u =>
      // do not send for non-test users for now
      !isLive && config.testNotificationsUserIds.contains(u.userId)
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
