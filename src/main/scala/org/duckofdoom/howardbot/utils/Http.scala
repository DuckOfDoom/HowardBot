package org.duckofdoom.howardbot.utils

import scala.concurrent.{ExecutionContext, Future}
import cats.syntax.either._

trait HttpService {
  def makeRequestAsync(url: String)(implicit executionContext : ExecutionContext) : Future[Either[String, String]]
}

class ScalajHttpService extends HttpService {
  import scalaj.http._

  override def makeRequestAsync(url: String)(implicit executionContext: ExecutionContext): Future[Either[String, String]] = {
    Future {

      val response: HttpResponse[String] = Http(url).asString
      if (response.code == 200)
        response.body.asRight
      else
        s"Got ${response.code} while processing http request '$url'".asLeft
    }
  }
}
