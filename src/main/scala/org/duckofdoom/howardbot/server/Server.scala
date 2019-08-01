package org.duckofdoom.howardbot.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.BotStatus
import slogging.StrictLogging

import scala.concurrent.{ExecutionContextExecutor, Future}

class Server(implicit botStatus: BotStatus, responseService: ServerResponseService)
    extends StrictLogging {

  def run(implicit config: Option[Config]): Future[Unit] = {

    if (config.isEmpty) {
      return Future.failed(new Exception("No server configuration provided!"))
    }

    if (!config.get.startServer) {
      logger.info("Skipping server start.")
      return Future.successful()
    }

    implicit val system: ActorSystem                        = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer            = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    def respond(html: String) = {
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, html))
    }

    val route =
      get {
        concat(
          pathSingleSlash {
            respond(responseService.home())
          },
          path("menu") {
            respond(responseService.menu())
          },
          pathPrefix("users") {
            concat(
              pathEnd {
                respond(responseService.getUsers())
              },
              path("new") {
                respond(responseService.putRandomUser())
              },
              path(IntNumber) { userId =>
                respond(responseService.getUser(userId))
              }
            )
          },
          pathPrefix("show") {
            path(IntNumber) { itemId =>
              respond(responseService.show(itemId))
            }
          },
        )
      }

    val address = config.get.serverAddress
    val port    = config.get.serverPort

    logger.info(
      s"""Starting server at http://$address:$port
            http://$address:$port/menu
            http://$address:$port/users
            http://$address:$port/users/new
        """
    )
    
    Http().bindAndHandle(route, address, port)
    Future.unit
  }

}
