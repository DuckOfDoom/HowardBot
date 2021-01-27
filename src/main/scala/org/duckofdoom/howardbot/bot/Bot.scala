package org.duckofdoom.howardbot.bot

import java.time.Duration

import scala.concurrent.Future

trait Bot {
  def sendNotification(userId: Int, title: String, message: String): Future[Unit]
  def runningTime: Duration
  def run(): Future[Unit]
}
