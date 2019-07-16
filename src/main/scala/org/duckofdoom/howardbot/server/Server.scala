package org.duckofdoom.howardbot.server

import cats.effect.IO
import com.twitter.finagle.Http
import com.twitter.util.Await
import io.finch.{Endpoint, Ok, Text}

object Server extends Endpoint.Module[IO] {
  
  def run()  {
    val api: Endpoint[IO, String] = get("hello") {
      Ok("Hello, World!")
    }

    Await.ready(Http.server.serve(":8080", api.toServiceAs[Text.Plain]))
  }

}
