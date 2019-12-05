package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.option._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.ItemType
import org.duckofdoom.howardbot.bot.services.{ResponseFormat, ResponseService}
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User
import org.duckofdoom.howardbot.utils.Extensions._
import org.duckofdoom.howardbot.utils.Extractors._
import slogging.StrictLogging

import scala.concurrent.Future

class HowardBot(val config: Config)(implicit responseService: ResponseService, db: DB)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future]
    with Callbacks[Future] {

  type TelegramUser = com.bot4s.telegram.models.User

  override val client: RequestHandler[Future] = new CustomScalajHttpClient(config.token)

  /*
    Sends a message to a specified user
   */
  def sendNotification(userId: Int, title: String, message: String): Future[Unit] = {
    logger.info(f"Sending message to user $userId: $title -  $message")

    request(
      SendMessage(
        ChatId(userId),
        responseService.formatNotification(title, message),
        parseMode = ParseMode.HTML.some
      )
    ).void
  }

  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkMenuResponse(
        u.state.menuPage,
        u.state.sorting
      )

      (respond(items, markup.some)(msg), u)
    }
  }

  onCommand("styles") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkStylesResponse(1)
      (respond(items, markup.some)(msg), u)
    }
  }

  onCommand("sort") { implicit msg =>
    withUser(msg.chat) { u =>
      val modifiedUser    = u.withEmptySorting()
      val (items, markup) = responseService.mkChangeSortingResponse(u.state.sorting)
      (respond(items, markup.some)(msg), modifiedUser)
    }
  }

  // When users clicks any of the buttons
  onCallbackQuery { implicit query =>
    withUser(query.from) { u =>
      var user = u
      val responseFuture = (query.data, query.message) match {
        case (Some(data), Some(msg)) =>
          implicit val message: Message = msg

          Callback.deserialize(data.getBytes) match {
            // Sent from "Menu" button
            case Some(Callback.Menu(page, newMessage)) =>
              logger.info(
                s"Received 'Menu' callback from user @${user.username}, page: $page, newMessage: $newMessage"
              )

              if (page.isDefined) {
                user = user.withMenuPage(page.get)
              }

              respond(
                responseService.mkMenuResponse(user.state.menuPage, user.state.sorting),
                newMessage
              ).some

            // Sent from "Styles" button
            case Some(Callback.Styles(page, newMessage)) =>
              logger.info(
                s"Received 'Styles' callback from user @${user.username}, page: $page, newMessage: $newMessage"
              )

              if (page.isDefined) {
                user = user.withMenuPage(page.get)
              }

              respond(
                responseService.mkStylesResponse(user.state.menuPage),
                newMessage
              ).some

            // Sent from clicking on a particular style button
            case Some(Callback.ItemsByStyle(style, page)) =>
              logger.info(
                s"Received 'ItemsByStyle' callback from user @${user.username}, style: $style, page: $page"
              )

              respond(
                responseService.mkBeersByStyleResponse(style, page, user.state.sorting),
                newMessage = false
              ).some

            // Sent from clicking on a particular item button (either beer or style)
            case Some(Callback.SingleBeer(itemType, itemId)) =>
              logger.info(
                s"Received 'Item' callback from user @${user.username}, itemType: $itemType, itemId: $itemId"
              )

              itemType match {
                case ItemType.Beer =>
                  respond(
                    responseService.mkBeerResponse(itemId),
                    newMessage = true
                  ).some
                case ItemType.Style =>
                  respond(
                    responseService.mkBeersByStyleResponse(itemId, 1, user.state.sorting),
                    newMessage = true
                  ).some
              }
            case Some(Callback.SearchBeerByStyle(searchQuery, page)) =>
              logger.info(
                s"Received 'SearchBeerByStyle' callback from user @${user.username}, query: '$searchQuery', page: $page"
              )

              respond(
                responseService.mkSearchBeerByStyleResponse(searchQuery, page, user.state.sorting),
                newMessage = false
              ).some
            case Some(Callback.SearchBeerByName(searchQuery, page)) =>
              logger.info(
                s"Received 'SearchBeerByName' callback from user @${user.username}, query: '$searchQuery', page: $page"
              )

              respond(
                responseService.mkSearchBeerByNameResponse(searchQuery, page, user.state.sorting),
                newMessage = false
              ).some

            case Some(Callback.ChangeSorting(mSorting)) =>
              logger.info(
                s"Received 'ChangeSorting' callback from user @${user.username}, sorting: $mSorting"
              )

              if (mSorting.isRight) {

                // Reset menu page because we're going to change sorting and it wont make any sense.
                user = user.withMenuPage(1)

                val sorting = mSorting.right.get
                if (sorting.isEmpty) {
                  user = user.withEmptySorting()
                } else {
                  user = user.withAddedSorting(sorting.get)
                }
              }

              respond(
                responseService.mkChangeSortingResponse(user.state.sorting),
                newMessage = mSorting.isLeft
              ).some
            case _ =>
              None
          }
        case _ => None
      }

      if (responseFuture.isEmpty)
        logger.error(s"Failed to construct response for callback query: ${query.data}")

      // Combine our response with ACK since we should always acknowledge callback query
      val responseFutureWithAck: Future[Unit] = for {
        ack    <- ackCallback()
        answer <- responseFuture.getOrElse(Future.successful())
      } yield (ack, answer)

      (responseFutureWithAck, user)
    }
  }

  override def receiveMessage(msg: Message): Future[Unit] = {

    implicit val message: Message = msg

    if (msg.text.isEmpty) {
      logger.warn("Received empty text message.")
      return super.receiveMessage(msg)
    }

    withUser(msg.chat) { user =>
      implicit val format: ResponseFormat.Value = ResponseFormat.TextMessage

      val responseFuture = msg.text.get match {
        case Consts.showItemRegex(Int(id)) =>
          val (item, markup) = responseService.mkBeerResponse(id)
          request(
            SendMessage(
              ChatId(msg.source),
              item,
              ParseMode.HTML.some,
              // Only this request should show links
              false.some,
              None,
              None,
              markup.some
            )
          ).void
        case Consts.showItemsByStyleRegex(Int(styleId)) =>
          respond(
            responseService.mkBeersByStyleResponse(styleId, 1, user.state.sorting),
            newMessage = true
          ).void
        case Consts.SearchBeerByStyleQuery(query) =>
          respond(
            responseService.mkSearchBeerByStyleResponse(query, 1, user.state.sorting),
            newMessage = true
          ).void
        case Consts.SearchBeerByNameQuery(query) =>
          respond(
            responseService.mkSearchBeerByNameResponse(query, 1, user.state.sorting),
            newMessage = true
          ).void
        // Treat simple text as beer-by-name search
        case _ =>
          if (msg.text.get.startsWith("/")) {
            return super.receiveMessage(msg)
          }

          respond(
            responseService.mkSearchBeerByNameResponse(msg.text.get, 1, user.state.sorting),
            newMessage = true
          ).void
      }

      (responseFuture, user)
    }
  }

  override def receiveUpdate(u: Update, botUser: Option[TelegramUser]): Future[Unit] = {
    try {
      super.receiveUpdate(u, botUser)
    } catch {
      case ex: Throwable =>
        logger.error(s"Caught exception when receiving update:\n${ex.toStringFull}")
        Future.failed(ex)
    }
  }

  /**
    * Allows to pull user that sent a message from DB when processing queries.
    * 'action' should always return either a supplied User or modified User if we need to modify it.
    * If returned user does not equal the provided one, said user is updated in database.
    */
  private def withUser(tgUser: TelegramUser)(action: User => (Future[Unit], User)): Future[Unit] = {

    if (tgUser.firstName.isEmpty) {
      return Future.failed(
        new Exception(
          "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."
        )
      )
    }

    val user = db.getUserByTelegramId(tgUser.id) match {
      case Some(u) => u.some
      case _ =>
        db.putUser(
          tgUser.id,
          tgUser.firstName,
          tgUser.lastName,
          tgUser.username
        )
    }

    user match {
      case Some(u) =>
        val (future, modifiedUser) = action(u)
        if (modifiedUser != u)
          db.updateUser(modifiedUser)
        future
      case _ =>
        Future.failed(
          new Exception(
            s"Failed to process action! Could not create user for telegramUser: $tgUser"
          )
        )
    }
  }

  /**
    * Allows to pull user that sent a message from DB when processing queries.
    * 'action' should always return either a supplied User or modified User if we need to modify it.
    * If returned user does not equal the provided one, said user is updated in database.
    */
  private def withUser(chat: Chat)(action: User => (Future[Unit], User)): Future[Unit] = {
    if (chat.firstName.isEmpty) {
      return Future.failed(
        new Exception(
          "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."
        )
      )
    }

    val user = db.getUserByTelegramId(chat.id) match {
      case Some(u) => u.some
      case _ =>
        db.putUser(
          chat.id,
          chat.firstName.get,
          chat.lastName,
          chat.username
        )
    }

    user match {
      case Some(u) =>
        val (future, modifiedUser) = action(u)
        if (modifiedUser != u)
          db.updateUser(modifiedUser)
        future
      case _ =>
        Future.failed(
          new Exception(s"Failed to process action! Could not create user for chat: $chat")
        )
    }
  }

  private def respond(text: String, markup: Option[ReplyMarkup] = None)(
      implicit message: Message
  ) = {

    reply(
      text,
      ParseMode.HTML.some,
      true.some,
      None,
      None,
      markup
    ).void
  }

  private def respond(response: (String, InlineKeyboardMarkup), newMessage: Boolean)(
      implicit msg: Message
  ) = {

    val (items, buttons) = response

    if (newMessage) {
      request(
        SendMessage(
          ChatId(msg.source),
          items,
          ParseMode.HTML.some,
          true.some,
          None,
          None,
          buttons.some
        )
      )
    } else {
      request(
        EditMessageText(
          ChatId(msg.source).some,
          msg.messageId.some,
          None,
          items,
          ParseMode.HTML.some,
          true.some,
          buttons.some
        )
      )
    }
  }

}
