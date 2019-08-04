package org.duckofdoom.howardbot.utils

import scala.concurrent.{ExecutionContext, Future}

trait HttpService {
  def makeRequestAsync(url: String)(implicit executionContext : ExecutionContext) : Future[String]
}

class ScalajHttpService extends HttpService {
  import scalaj.http._

  override def makeRequestAsync(url: String)(implicit executionContext: ExecutionContext): Future[String] = {
    Future {

      val response: HttpResponse[String] = Http(url).asString
//      response.body
//      response.code
//      response.headers
//      response.cookies

      response.body
    }
  }
}
