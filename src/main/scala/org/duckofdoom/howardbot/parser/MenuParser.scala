package org.duckofdoom.howardbot.parser

import java.util.Optional

import cats.syntax.either._
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data.Item
import slogging.StrictLogging
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object MenuParser extends StrictLogging {

  def parseMenu(contents: String): Either[String, List[Item]] = {

    val htmlLineName = "container.innerHTML = \"  "

    val menuHtml: Option[String] = toOption(
      contents.lines
        .filter(l => l.contains(htmlLineName))
        .findFirst())
      .map(_.substring(htmlLineName.length))
      .map(s => s.substring(0, s.length - 2))
      .map(_.replace("\\n", "\n"))

    if (menuHtml.isEmpty) {
      return s"Can't find '$htmlLineName' in provided string!".asLeft
    }

    print(menuHtml)

    val browser = JsoupBrowser()
    val doc     = browser.parseString(menuHtml.get)

    List[Item]().asRight
  }

  private def toOption[T](v: java.util.Optional[T]): Option[T] = {
    if (v.isPresent)
      v.get.some
    else
      Option.empty[T]
  }
}
