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
import org.duckofdoom.howardbot.bot.ResponseFormat.ResponseFormat
import org.duckofdoom.howardbot.bot.data.ItemType
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

  // TODO: Move command literals to separate file
  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkMenuResponse(
        u.state.menuPage,
        u.state.sorting
      )(ResponseFormat.TextMessage)

      respond(items, markup.some)(msg)
    }
  }

  onCommand("styles") { implicit msg =>
    withUser(msg.chat) { _ =>
      val (items, markup) = responseService.mkStylesResponse(1)(ResponseFormat.Buttons)
      respond(items, markup.some)(msg)
    }
  }

  onCommand("sort") { implicit msg =>
    withUser(msg.chat) { u =>
      u.state.sorting = List()
      val (items, markup) = responseService.mkChangeSortingResponse(u.state.sorting)
      respond(items, markup.some)(msg)
    }
  }

  // When users clicks any of the buttons
  onCallbackQuery { implicit query =>
    withUser(query.from) { u =>
      val responseFuture = (query.data, query.message) match {
        case (Some(data), Some(msg)) =>
          implicit val message: Message       = msg
          implicit val format: ResponseFormat = ResponseFormat.TextMessage

          Callback.deserialize(data.getBytes) match {
            // Sent from "Menu" button
            case Some(Callback.Menu(page, newMessage)) =>
              logger.info(
                s"Received 'Menu' callback from user @${u.username}, page: $page, newMessage: $newMessage"
              )

              if (page.isDefined) {
                u.state.menuPage = page.get
              }

              respond(
                responseService.mkMenuResponse(u.state.menuPage, u.state.sorting),
                newMessage
              ).some

            // Sent from "Styles" button
            case Some(Callback.Styles(page, newMessage)) =>
              logger.info(
                s"Received 'Styles' callback from user @${u.username}, page: $page, newMessage: $newMessage"
              )

              if (page.isDefined) {
                u.state.menuPage = page.get
              }

              respond(
                responseService.mkStylesResponse(u.state.menuPage)(ResponseFormat.Buttons),
                newMessage
              ).some

            // TODO: Add page option for new message
            // TODO: Right now it always responds as a new message
            // Sent from clicking on a particular style button
            case Some(Callback.ItemsByStyle(style, page)) =>
              logger.info(
                s"Received 'ItemsByStyle' callback from user @${u.username}, style: $style, page: $page"
              )

              respond(
                responseService.mkBeersByStyleResponse(style, page, u.state.sorting),
                newMessage = false
              ).some

            // Sent from clicking on a particular item button (either beer or style)
            case Some(Callback.Item(itemType, itemId)) =>
              logger.info(
                s"Received 'Item' callback from user @${u.username}, itemType: $itemType, itemId: $itemId"
              )

              itemType match {
                case ItemType.Beer =>
                  respond(
                    responseService.mkBeerResponse(itemId),
                    newMessage = true
                  ).some
                case ItemType.Style =>
                  respond(
                    responseService.mkBeersByStyleResponse(itemId, 1, u.state.sorting),
                    newMessage = true
                  ).some
              }
            case Some(Callback.SearchBeerByStyle(searchQuery, page)) =>
              logger.info(
                s"Received 'SearchBeerByStyle' callback from user @${u.username}, query: '$searchQuery', page: $page"
              )

              respond(
                responseService.mkSearchBeerByStyleResponse(searchQuery, page, u.state.sorting),
                newMessage = false
              ).some
            case Some(Callback.SearchBeerByName(searchQuery, page)) =>
              logger.info(
                s"Received 'SearchBeerByName' callback from user @${u.username}, query: '$searchQuery', page: $page"
              )

              respond(
                responseService.mkSearchBeerByNameResponse(searchQuery, page, u.state.sorting),
                newMessage = false
              ).some

            case Some(Callback.ChangeSorting(mSorting)) =>
              logger.info(
                s"Received 'ChangeSorting' callback from user @${u.username}, sorting: $mSorting"
              )
              
              if (mSorting.isEmpty) {
                respond(
                  responseService.mkMenuResponse(1, u.state.sorting), // Reset page because sorting was changed
                  newMessage = true
                ).some
              } else {
                
                u.state.sorting :+ mSorting.get
                
                respond(
                  responseService.mkChangeSortingResponse(u.state.sorting),
                  newMessage = false
                ).some
              }
            case _ =>
              None
          }
        case _ => None
      }

      if (responseFuture.isEmpty)
        logger.error(s"Failed to construct response for callback query: ${query.data}")

      for {
        ack    <- ackCallback()
        answer <- responseFuture.getOrElse(Future.successful())
      } yield (ack, answer)
    }
  }

  override def receiveMessage(msg: Message): Future[Unit] = {

    implicit val message: Message = msg
    

    if (msg.text.isEmpty) {
      logger.warn("Received empty text message.")
      return super.receiveMessage(msg)
    }

    if (msg.text.get.startsWith("/")) {
      return super.receiveMessage(msg)
    }
    
    withUser(msg.chat) { u =>
      msg.text.get match {
        case Consts.showItemRegex(Int(id)) =>
          val (item, markup) = responseService.mkBeerResponse(id)(ResponseFormat.TextMessage)
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
            responseService.mkBeersByStyleResponse(styleId, 1, u.state.sorting)(ResponseFormat.TextMessage),
            newMessage = true
          ).void
        case Consts.SearchBeerByStyleQuery(query) =>
          respond(
            responseService.mkSearchBeerByStyleResponse(query, 1, u.state.sorting)(ResponseFormat.TextMessage),
            newMessage = true
          ).void
        case Consts.SearchBeerByNameQuery(query) =>
          respond(
            responseService.mkSearchBeerByNameResponse(query, 1, u.state.sorting)(ResponseFormat.TextMessage),
            newMessage = true
          ).void
        // Treat simple text as beer-by-name search
        case _ =>
          respond(
            responseService.mkSearchBeerByNameResponse(msg.text.get, 1, u.state.sorting)(ResponseFormat.TextMessage),
            newMessage = true
          ).void
      }
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

  private def withUser(tgUser: TelegramUser)(action: User => Future[Unit]): Future[Unit] = {

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
        val prevUser = u.copy()
        val future   = action(u)
        if (u != prevUser)
          db.updateUser(u)
        future
      case _ =>
        Future.failed(
          new Exception(
            s"Failed to process action! Could not create user for telegramUser: $tgUser"
          )
        )
    }
  }

  private def withUser(chat: Chat)(action: User => Future[Unit]): Future[Unit] = {
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
      case Some(u) => action(u)
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
