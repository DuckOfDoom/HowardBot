package org.duckofdoom.howardbot.utils

import scala.concurrent.{ExecutionContext, Future}
import cats.syntax.option._
import slogging.StrictLogging

trait HttpService {
  def makeRequestAsync(url: String)(
      implicit executionContext: ExecutionContext): Future[Option[String]]
}

class ScalajHttpService extends HttpService with StrictLogging {
  import scalaj.http._

  override def makeRequestAsync(url: String)(
      implicit executionContext: ExecutionContext): Future[Option[String]] = {
    Future {
      logger.info(s"Requesting $url...")
      val response: HttpResponse[String] = Http(url).asString
      response.code match {
        case 200 => {
          if (response.body.isEmpty)
            logger.error(s"Got empty string as a response body! Response:\n$response")
          
          response.body.some
        }
        case _ =>
          logger.error(s"Got ${response.code} while processing http request '$url'")
          None
      }
    }
  }
}
