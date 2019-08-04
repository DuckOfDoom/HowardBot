package org.duckofdoom.howardbot.parser

import cats.syntax.either._
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data.{BreweryInfo, FakeItemDataProvider, Item, MenuItem}
import slogging.StrictLogging
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Element

import scala.collection.mutable.ListBuffer
import scala.util.Try

object MenuParser extends StrictLogging {

  def parseMenu(contents: String): List[Item] = {

    val htmlLineName = "container.innerHTML = \"  "

    val menuHtml: Option[String] = toOption(
      contents.lines
        .filter(l => l.contains(htmlLineName))
        .findFirst())
      .map(_.substring(htmlLineName.length))
      .map(s => s.substring(0, s.length - 2))
      .map(_.replace("\\n", "\n"))
      .map(_.replace("\\/", "/"))
      .map(_.replace("\\\"", "\""))

    if (menuHtml.isEmpty) {
      logger.error(s"Can't find '$htmlLineName' in provided string!")
      return List[Item]()
    }

    val browser = JsoupBrowser()
    val doc     = browser.parseString(menuHtml.get)

    (doc >> elementList(".beer")).zipWithIndex.map { case (el: Element, i: Int) => parseItem(i, el)}
  }

  def parseItem(id: Int, el: Element): Item = {
    val picLink = (el >?> element(".beer-label") >?> attr("src")("img")).flatten

    // beerName
    val beerName = el >?> element(".beer-name")
    val menuOrder = (beerName >?> element(".tap-number-hideable") >> text).flatten
      .map(_.takeWhile(_ != '.'))
      .flatMap(i => Try(i.toInt).toOption)

    val name = (beerName >?> element("a") >> text).flatten
      .map(_.dropWhile(!_.isLetter))

    val link  = (beerName >?> attr("href")("a")).flatten
    val style = (beerName >?> element(".beer-style") >> text).flatten

    // meta
    val metaEl = el >?> element(".item-meta")
    val abv = (metaEl >?> element(".abv") >> text).flatten
      .map(_.takeWhile(c => c.isDigit || c == '.'))
      .flatMap(i => Try(i.toFloat).toOption)

    val ibu = (metaEl >?> element(".ibu") >> text).flatten
      .map(_.takeWhile(c => c.isDigit || c == '.'))
      .flatMap(i => Try(i.toFloat).toOption)

    // brewery
    val breweryEl   = (metaEl >?> element(".brewery")).flatten
    val breweryLink = (breweryEl >?> attr("href")("a")).flatten

    val breweryName     = (breweryEl >?> element("a") >> text).flatten
    val breweryLocation = (metaEl >?> element(".location")).flatten >> text

    // description
    val description = (el >?> element(".item-description") >?> element("p")).flatten >> text

    // container
    val containerEl    = el >?> element(".container-list")
    val draftType      = (containerEl >?> element(".type")).flatten >> text
    val currencySymbol = (containerEl >?> element(".currency-hideable")).flatten >> text
    val price = (containerEl >?> element(".price") >> text).flatten
      .map(_.dropWhile(!_.isDigit))
      .map(_.replace(",", "")) // 1,700.00 rubley bldjad
      .flatMap(i => Try(i.toFloat).toOption)

    MenuItem(
      id,
      menuOrder,
      name,
      link,
      picLink,
      abv,
      ibu,
      BreweryInfo(breweryName, breweryLink, breweryLocation),
      style,
      draftType,
      for {
        s <- currencySymbol
        p <- price
      } yield (s, p),
      description
    )
  }
  
  private def toOption[T](v: java.util.Optional[T]): Option[T] = {
    if (v.isPresent)
      v.get.some
    else
      Option.empty[T]
  }

}
