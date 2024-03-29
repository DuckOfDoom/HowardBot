package org.duckofdoom.howardbot.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives.{path, _}
import akka.stream.ActorMaterializer
import org.duckofdoom.howardbot.Config
import slogging.StrictLogging

import scala.concurrent.{ExecutionContextExecutor, Future}

class Server(responseService: ServerResponseService) extends StrictLogging {

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
            respond(responseService.home)
          },
          
          pathPrefix("menu") {
            concat(
              path("available") {
                respond(responseService.menuAvailable)
              },
              path("ondeck") {
                respond(responseService.menuOnDeck)
              },
              path("outofstock") {
                respond(responseService.menuOutOfStock)
              },
              path("full") {
                respond(responseService.menuFull)
              },
              path("raw") {
                respond(responseService.menuRaw)
              },
              path("changelog") {
                respond(responseService.menuChangelog)
              },
              path("styles") {
                respond(responseService.styles)
              }
            )
          },
          pathPrefix("users") {
            concat(
              pathEnd {
                respond(responseService.users)
              },
              path("new") {
                respond(responseService.putRandomUser())
              },
              path(IntNumber) { id =>
                respond(responseService.getUser(id))
              }
            )
          },
          pathPrefix("show") {
            path(IntNumber) { itemId =>
              respond(responseService.show(itemId))
            }
          },
          pathPrefix ("notifications") {
            concat(
              pathEnd {
                respond(responseService.notificationsForm)
              },
              path("send") {
                parameters("title", "message", "isLive".as[Boolean] ? false) { (title, msg, isLive) =>
                  respond(responseService.sendNotification(title, msg, isLive))
                }
              }
            )
          }
        )
      }

    val address = config.get.serverAddress
    val port    = config.get.serverPort

    logger.info(s"Starting server at http://$address:$port")

    Http().bindAndHandle(route, address, port)
    Future.unit
  }

}
