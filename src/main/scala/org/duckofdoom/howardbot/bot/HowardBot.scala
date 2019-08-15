package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.option._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models.{Chat, ChatId, InlineKeyboardMarkup, Message, ReplyMarkup, Update}
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.db.DB
import org.duckofdoom.howardbot.db.dto.User
import slogging.StrictLogging

import scala.concurrent.Future
import scala.util.Try
import org.duckofdoom.howardbot.utils.Extensions._
import org.duckofdoom.howardbot.utils.Extractors._

class HowardBot(val config: Config)(implicit responseService: ResponseService, db: DB)
    extends TelegramBot
    with StrictLogging
    with Polling
    with Commands[Future]
    with Callbacks[Future] {

  type TelegramUser = com.bot4s.telegram.models.User

  // TODO: We need to serve multiple users in separate threads. Right now one user blocks everything =(
  override val client: RequestHandler[Future] = new CustomScalajHttpClient(config.token)

  override def receiveUpdate(u: Update, botUser: Option[TelegramUser]): Future[Unit] = {
    try {
      super.receiveUpdate(u, botUser)
    }
    catch {
      case ex: Throwable =>
        logger.error(s"Got Exception while receiving update: ${ex.toStringFull}")
        Future.successful()
    }
  }

  // TODO: Move command literals to separate file
  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkMenuResponsePaginated(
        u.state.menuPage
      )

      respond(items, markup.some)
    }
  }

  // TODO: Add command for searching by style
  onCommand("styles") { implicit msg =>
    withUser(msg.chat) { _ =>
      val (items, markup) = responseService.mkStylesResponse(1)
      respond(items, markup.some)
    }
  }

  onCallbackQuery { implicit query =>
    withUser(query.from) { u =>
  
      val responseFuture = (query.data, query.message) match {
        // Make response for the whole menu
        case (Some("menu"), Some(msg)) =>
          mkPaginatedResponse(u.state.menuPage, msg, newMessage = true) { p => 
          
            u.state.menuPage = p
            db.updateUser(u)
            
            responseService.mkMenuResponsePaginated(p)
          }.some

        // Make response for specific page when button is clicked
        case (Some(CallbackUtils.menuCallbackRegex(page)), Some(msg)) =>
          Try(page.toInt).toOption match {
            case Some(p) =>
              mkPaginatedResponse(p, msg, newMessage = false) { p =>
              
                u.state.menuPage = p
                db.updateUser(u)
                
                responseService.mkMenuResponsePaginated(p)
                
              }.some
            case _ =>
              logger.error(s"Failed to parse page from callback query data: ${query.data}")
              None
          }
        case (Some(CallbackUtils.stylesCallbackRegex(page)), Some(msg)) =>
          Try(page.toInt).toOption match {
            case Some(p) =>
              mkPaginatedResponse(p, msg, newMessage = false) { p =>
                responseService.mkStylesResponse(p)
              }.some
            case _ =>
              logger.error(s"Failed to parse page and args from callback query data: ${query.data}")
              None
          }
        case (Some(CallbackUtils.itemsByStyleCallbackRegex(style, page)), Some(msg)) =>
          Try(page.toInt).toOption match {
            case Some(p) =>
              mkPaginatedResponse(p, msg, newMessage = false) { p =>
                responseService.mkItemsByStyleResponse(p, style)
              }.some
            case _ =>
              logger.error(s"Failed to parse page and args from callback query data: ${query.data}")
              None
          }
        case _ => Future.successful().some
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
    
    if (msg.text.isEmpty) {
      logger.warn("Received empty text message.")
      return super.receiveMessage(msg)
    }
    
    msg.text.get match {
      case Consts.showItemRegex(Int(id)) =>
        val (item, markup) = responseService.mkItemResponse(id)
        request(
          SendMessage(ChatId(msg.source),
            item,
            ParseMode.HTML.some,
            true.some,
            None,
            None,
            markup.some)
        ).void
      case _ => super.receiveMessage(msg)
    }
  }

  private def withUser(tgUser: TelegramUser)(action: User => Future[Unit]): Future[Unit] = {

    if (tgUser.firstName.isEmpty) {
      return Future.failed(new Exception(
        "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."))
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
      case Some(u) => action(u)
      case _ =>
        Future.failed(
          new Exception(
            s"Failed to process action! Could not create user for telegramUser: $tgUser"))
    }
  }

  private def withUser(chat: Chat)(action: User => Future[Unit]): Future[Unit] = {
    if (chat.firstName.isEmpty) {
      return Future.failed(new Exception(
        "chat.firstName is empty, meaning this is not 1 on 1 chat. Support for these is not implemented yet."))
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
          new Exception(s"Failed to process action! Could not create user for chat: $chat"))
    }
  }

  private def respond(text: String, markup: Option[ReplyMarkup] = None)(
      implicit message: Message) = {

    reply(
      text,
      ParseMode.HTML.some,
      true.some,
      None,
      None,
      markup,
    ).void
  }


  def mkPaginatedResponse(page: Int, msg: Message, newMessage: Boolean)(mkResponse: Int => (String, InlineKeyboardMarkup)) = {
    
    val (items, buttons) = mkResponse(page)

    if (newMessage) {
      request(
        SendMessage(ChatId(msg.source),
          items,
          ParseMode.HTML.some,
          true.some,
          None,
          None,
          buttons.some)
      )
    } else {
      request(
        EditMessageText(ChatId(msg.source).some,
          msg.messageId.some,
          None,
          items,
          ParseMode.HTML.some,
          true.some,
          buttons.some)
      )
    }
  }

}
