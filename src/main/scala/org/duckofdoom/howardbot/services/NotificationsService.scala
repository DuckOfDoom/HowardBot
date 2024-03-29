package org.duckofdoom.howardbot.services

import org.duckofdoom.howardbot.bot.Bot
import org.duckofdoom.howardbot.db.DB
import slogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/* Service that is used to send notifications for users */

class NotificationsService(
    testNotificationsUserIds: Set[Int],
    menuUpdatesNotificationsUserIds: Set[Int],
    db: DB,
    bot: Bot
)(implicit ec: ExecutionContext)
    extends StrictLogging {

  logger.info(s"""Created. testNotificationsUserIds:$testNotificationsUserIds,
                 menuUpdatesNotificationsUserIds:$menuUpdatesNotificationsUserIds
                 """)

  /*
   Sends notifications to users.
   Returns a list of errors for each user.
   */
  def sendNotification(title: String, message: String, isLive: Boolean): Future[Seq[String]] = {

    val usersToSendMessages = db.users.filter { u =>
      // do not send for non-test users for now
      !isLive && testNotificationsUserIds.contains(u.userId) ||
      isLive && u.state.notificationsEnabled
    }

    logger.info(f"Sending message: $title - $message to ${usersToSendMessages.length} users.")

    Future.sequence(
      usersToSendMessages.map { u =>
        {
          val strUser = f"${u.firstName} (${u.userId} - ${u.username})"

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

  /*
    Sends notification to specified users about menu updates
   */
  def sendMenuUpdates(updates: Seq[String]): Unit = {
    if (updates.isEmpty)
      return

    val usersToSendMessages = db.users.filter { u =>
      menuUpdatesNotificationsUserIds.contains(u.userId)
    }

    logger.info(f"Sending menu updates to ${usersToSendMessages.length} users.")

    Future.sequence(
      usersToSendMessages.map { u =>
        {
          val strUser = f"${u.firstName} (${u.userId} - ${u.username})"

          //   TODO: Process errors where this is called?
          bot.sendNotification(u.userId, "Обновление меню:", updates.mkString("\n")).transform {
            case Success(_) =>
              val msg = f"Successfully sent menu update notification to $strUser"
              logger.info(msg)
              Success(msg)
            case Failure(ex) =>
              val msg = f"Failed to send menu update notification to $strUser because of error: ${ex.getMessage}"
              logger.error(msg)
              Success(msg)
          }
        }
      }
    )

  }
}
