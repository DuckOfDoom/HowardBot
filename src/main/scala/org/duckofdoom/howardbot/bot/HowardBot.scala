package org.duckofdoom.howardbot.bot

import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.option._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models._
import io.circe.generic.auto._
import io.circe.parser.decode
import org.duckofdoom.howardbot.Config
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

  // TODO: We need to serve multiple users in separate threads. Right now one user blocks everything =(
  override val client: RequestHandler[Future] = new CustomScalajHttpClient(config.token)

  // TODO: Move command literals to separate file
  onCommand("start" | "menu") { implicit msg =>
    withUser(msg.chat) { u =>
      val (items, markup) = responseService.mkMenuResponse(
        u.state.menuPage
      )

      respond(items, markup.some)
    }
  }

  onCommand("styles") { implicit msg =>
    withUser(msg.chat) { _ =>
      val (items, markup) = responseService.mkStylesResponse(1)
      respond(items, markup.some)
    }
  }

  onCallbackQuery { implicit query =>
    logger.info(s"Received callback query: ${query.data}.")

    withUser(query.from) { u =>
      val responseFuture = (query.data, query.message) match {
        case (Some(data), Some(msg)) =>
          decode[Callback](data) match {
            case Right(Callback.Menu(page, newMessage)) =>
              respondWithCallbackButtons(page.getOrElse(u.state.menuPage), msg, newMessage) {
                p =>
                  u.state.menuPage = p
                  db.updateUser(u)
                  responseService.mkMenuResponse(p)
              }.some

            case Right(Callback.Styles(page, newMessage)) =>
              respondWithCallbackButtons(page.getOrElse(u.state.stylesPage), msg, newMessage) {
                p =>
                  u.state.stylesPage = p
                  db.updateUser(u)
                  responseService.mkStylesResponse(p)
              }.some

            case Right(Callback.ItemsByStyle(style, page)) =>
              respondWithCallbackButtons(page, msg, newMessage = false) { p =>
                responseService.mkItemsByStyleResponse(style, page)
              }.some

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
                      // Only this request should show links
                      false.some,
                      None,
                      None,
                      markup.some)
        ).void
      case Consts.showItemsByStyleRegex(Int(styleId)) =>
        respondWithCallbackButtons(1, msg, newMessage = true) { p =>
          responseService.mkItemsByStyleResponse(styleId, p)
        }.void
      case _ => super.receiveMessage(msg)
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

  private def respondWithCallbackButtons(page: Int, msg: Message, newMessage: Boolean)(
      mkResponse: Int => (String, InlineKeyboardMarkup)) = {

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
