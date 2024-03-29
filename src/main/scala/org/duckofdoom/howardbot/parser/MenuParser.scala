package org.duckofdoom.howardbot.parser

import java.time.LocalDateTime

import cats.syntax.option._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import slogging.StrictLogging

import scala.util.Try
import scala.util.matching.Regex

class MenuParser(scriptOutput: String, additionalMenuPages: Seq[String]) extends StrictLogging {

  private val parsedBeersByName = scala.collection.mutable.Map[String, Beer.ParsedInfo]()
  private val browser           = JsoupBrowser()

  def parse(): Seq[Beer.ParsedInfo] = {
    parseScriptOutput(scriptOutput)
    logger.info(s"Parsed ${parsedBeersByName.count(_ => true)} items from main script output.")
    val parsedBeersCount = parsedBeersByName.count(_ => true)
    parseAdditionalPages(additionalMenuPages, parsedBeersCount)
    logger.info(s"""Parsed ${parsedBeersByName
      .count(_ => true) - parsedBeersCount} more items from additional pages.
          Total items: ${parsedBeersByName.count(_ => true)}
        """)

    parsedBeersByName.values.toList
  }

  private def parseScriptOutput(contents: String): Unit = {
    logger.info("Parsing main script output...")
    val htmlLineName = "container.innerHTML = \"  "
    val menuHtml: Option[String] = toOption(
      contents.lines
        .filter(l => l.contains(htmlLineName))
        .findFirst())
      .map(_.substring(htmlLineName.length))
      .map(cleanHtml)
      .map(s => s.substring(0, s.length - 2))

    if (menuHtml.isEmpty) {
      logger.error(s"Can't find '$htmlLineName' in provided string!")
      return
    }

    val doc      = browser.parseString(menuHtml.get)
    val elements = doc >> elementList(".beer")

    elements.zipWithIndex
      .map { case (el: Element, i: Int) => parseBeer(i, el) }
      .foreach(addParsedInfo)
  }

  private def parseAdditionalPages(pages: Seq[String], startingId: Int): Unit = {

    logger.info(s"Parsing ${pages.length} additional pages...")

    var id = startingId

    // Uncomment to debug additional pages.
//    pages.zipWithIndex.foreach { case (p: String, i: Int) => FileUtils.writeFile(s"page_$i", p) }

    pages
      .map(cleanHtml)
      .foreach(html => {
        val doc = browser.parseString(html)
        (doc >> elementList(".beer"))
          .map(el => {
            val item = parseBeer(id, el)
            id += 1
            item
          })
          .foreach(addParsedInfo)
      })
  }

  private def addParsedInfo(item: Beer.ParsedInfo): Unit = {
    if (item.name.isEmpty) {
      logger.trace(s"Can't add parsed item because name is not defined:\n$item")
      return
    }

    val n = item.name.getOrElse("_")
    if (!parsedBeersByName.contains(n)) {
      parsedBeersByName(n) = item
      logger.trace("New item: " + item.name)
    } else {
      logger.trace("Already has: " + item.name)
    }
  }

  private def cleanHtml(html: String) = {
    html
      .replace("\\n", "\n")
      .replace("\\/", "/")
      .replace("\\\"", "\"")
  }

  val ratingRegex: Regex = "rating small r(\\d{3})".r
  val cleanNameRegex: Regex = """\d+\.\s""".r

  private def parseBeer(id: Int, el: Element): Beer.ParsedInfo = {
    val picLink = (el >?> element(".beer-label") >?> attr("src")("img")).flatten

    // beerName
    val beerName = el >?> element(".beer-name")
    val menuOrder = (beerName >?> element(".tap-number-hideable") >> text).flatten
      .map(_.takeWhile(_ != '.'))
      .flatMap(i => Try(i.toInt).toOption)

    val rating = (el >?> element(".rating-hideable"))
      .flatMap(_ >?> element("span"))
      .flatMap(_.attrs.get("class"))
      .flatMap(
        v =>
          ratingRegex
            .findFirstMatchIn(v)
            .map(_.group(1))
            .flatMap(s => Try(s.toFloat / 100f).toOption))
      .map(v => (v, 5f))

    val name = (beerName >?> element("a") >> text).flatten
      .map(s => cleanNameRegex.replaceAllIn(s, ""))

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
    
    Beer.ParsedInfo(
      LocalDateTime.now(),
      name,
      menuOrder,
      rating,
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
