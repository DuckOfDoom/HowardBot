package org.duckofdoom.howardbot.bot.services

import scala.concurrent.{ExecutionContext, Future}

trait MenuRefreshService {
  def startRefreshLoop(onChanged: Seq[String] => Unit)(implicit ec: ExecutionContext): Future[Unit]
}


