package org.duckofdoom.howardbot.server

import java.time.{Duration, LocalTime}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.duckofdoom.howardbot.bot.Bot
import slogging.StrictLogging

import scala.concurrent.{ExecutionContextExecutor, Future}

object Server extends StrictLogging {

  def run(): Future[Unit] = {
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val route =
      path("") {
        get {
          implicit val runningTime: Duration = Duration.between(Bot.startupTime, LocalTime.now())
          implicit val restartCount: Int = Bot.restartCount
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, Pages.homePage))
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    logger.info(s"Server online at http://localhost:8080/")

    //    bindingFuture
    //      .flatMap(_.unbind()) // trigger unbinding from the port
    //      .onComplete(_ => system.terminate()) 

    Future.unit
  }
}
