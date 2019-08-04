package org.duckofdoom.howardbot.parser

import java.util.Optional

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

  def parseMenu(contents: String): Either[String, List[Item]] = {

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
      return s"Can't find '$htmlLineName' in provided string!".asLeft
    }

//    print(menuHtml)

    val browser = JsoupBrowser()
    val doc     = browser.parseString(menuHtml.get)

    val items = (doc >> elementList(".beer")).map(parseItem)
    val filtered = items.filter(_.isDefined).map(_.get)
    
    println(filtered.length)
    
    filtered.asRight
  }

  private def toOption[T](v: java.util.Optional[T]): Option[T] = {
    if (v.isPresent)
      v.get.some
    else
      Option.empty[T]
  }

  def parseItem(el: Element): Option[Item] = {
    val picLink = (el >?> element(".beer-label") >?> attr("src")("img")).flatten
    println(picLink)

    // beerName
    val beerName = el >?> element(".beer-name")
    
    val id = (beerName >?> element(".tap-number-hideable") >> text).flatten
      .map(_.takeWhile(_ != '.'))
      .flatMap(i => Try(i.toInt).toOption)
    println(id)

    val name = (beerName >?> element("a") >> text).flatten
      .map(_.dropWhile(!_.isLetter))

    println(name)

    val link = (beerName >?> attr("href")("a")).flatten
    println(link)

   
    val style = (beerName >?> element(".beer-style") >> text).flatten
    println(style)

    // meta
    val metaEl = el >?> element(".item-meta")
    val abv = (metaEl >?> element(".abv") >> text).flatten
      .map(_.takeWhile(c => c.isDigit || c == '.'))
      .flatMap(i => Try(i.toFloat).toOption)
    println(abv)

    val ibu = (metaEl >?> element(".ibu") >> text).flatten
      .map(_.takeWhile(c => c.isDigit || c == '.'))
      .flatMap(i => Try(i.toInt).toOption)
    println(ibu)

    // brewery
    val breweryEl   = (metaEl >?> element(".brewery")).flatten
    val breweryLink = (breweryEl >?> attr("href")("a")).flatten
    println(breweryLink)
    val breweryName = (breweryEl >?> element("a") >> text).flatten
    println(breweryName)
    val breweryLocation = (metaEl >?> element(".location")).flatten >> text
    println(breweryLocation)

    // description

    val description = (el >?> element(".item-description") >?> element("p")).flatten >> text
    println(description)

    // container
    val containerEl = el >?> element(".container-list")
    val draftType   = (containerEl >?> element(".type")).flatten >> text
    println(draftType)

    val currencySymbol = (containerEl >?> element(".currency-hideable")).flatten >> text
    println(currencySymbol)

    val price = (containerEl >?> element(".price") >> text).flatten
      .map(_.dropWhile(!_.isDigit))
      .flatMap(i => Try(i.toFloat).toOption)

    println(price)
    //    print(beerNameClass)

    for {
      id              <- id
      name            <- name
      link            <- link
      picLink         <- picLink
      style           <- style
      abv             <- abv
      ibu             <- ibu
      breweryName     <- breweryName
      breweryLink     <- breweryLink
      breweryLocation <- breweryLocation
      description     <- description
      draftType       <- draftType
      currencySymbol  <- currencySymbol
      price           <- price
    } yield
      MenuItem(
        id,
        name,
        link,
        picLink,
        abv,
        ibu,
        BreweryInfo(breweryName, breweryLink, breweryLocation),
        style,
        draftType,
        (currencySymbol, price),
        description
      )
  }
}
