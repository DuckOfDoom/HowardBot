package org.duckofdoom.howardbot.server

import java.time.{Duration, LocalTime}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.Bot
import slogging.StrictLogging

import scala.concurrent.{ExecutionContextExecutor, Future}

object Server extends StrictLogging {

  def run(implicit config:Option[Config]): Future[Unit] = {

    if (config.isEmpty) {
      return Future.failed(new Exception("No server configuration provided!"))
    }
    
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val route =
      path("") {
        get {
          implicit val runningTime: Duration = Duration.between(Bot.startupTime, LocalTime.now())
          implicit val restartCount: Int = Bot.restartCount
          implicit val restartReason: Option[String] = Bot.lastRestartReason
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, Pages.homePage))
        }
      }

    val address = config.get.serverAddress
    val port = config.get.serverPort
    Http().bindAndHandle(route, address, port)

    logger.info(s"Started server at http://$address:$port")

    //    bindingFuture
    //      .flatMap(_.unbind()) // trigger unbinding from the port
    //      .onComplete(_ => system.terminate()) 

    Future.unit
  }
}
