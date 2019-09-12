package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot
import org.duckofdoom.howardbot.bot.data.Beer

import scala.util.Try
import scala.util.matching.Regex

import util.control.Breaks._

object Sorting extends Enumeration {

  type Sorting = Value

  val byName: bot.Sorting.Value    = Value("n")
  val byNameDec: bot.Sorting.Value = Value("nd")

  val byStyle: bot.Sorting.Value    = Value("sd")
  val byStyleDec: bot.Sorting.Value = Value("sd")

  val byRating: bot.Sorting.Value    = Value("r")
  val byRatingDec: bot.Sorting.Value = Value("rd")

  val byPriceForMl: bot.Sorting.Value    = Value("p")
  val byPriceForMlDec: bot.Sorting.Value = Value("pd")

  val byBrewery: bot.Sorting.Value    = Value("b")
  val byBreweryDec: bot.Sorting.Value = Value("bd")

  val mlRegex: Regex = "(\\d+)\\s*ml".r
  val clRegex: Regex = "(\\d+)\\s*cl".r

  val all: Seq[Sorting] = {
    Seq(
      byName,
      byNameDec,
      byStyle,
      byStyleDec,
      byRating,
      byRatingDec,
      byPriceForMl,
      byPriceForMlDec,
      byBrewery,
      byBreweryDec
    )
  }

  class BeerOrdering(sortings: Seq[Sorting]) extends Ordering[Beer] {

    override def compare(x: Beer, y: Beer): Int = {
      var value = 0

      breakable {
        for (s <- sortings) {
          // Only compare next if values are equal
          if (value != 0)
            break

          s match {
            case Sorting.byName    => value = compareOption(x.name, y.name)
            case Sorting.byNameDec => value = compareOption(y.name, x.name)

            case Sorting.byStyle    => value = compareOption(x.style, y.style)
            case Sorting.byStyleDec => value = compareOption(y.style, x.style)

            case Sorting.byRating => value = compareOption(x.rating.map(_._1), y.rating.map(_._1))
            case Sorting.byRatingDec =>
              value = compareOption(y.rating.map(_._1), x.rating.map(_._1))

            case Sorting.byPriceForMl => value = compareOption(getPriceForMl(x), getPriceForMl(y));
            case Sorting.byPriceForMlDec =>
              value = compareOption(getPriceForMl(y), getPriceForMl(x));

            case Sorting.byBrewery => value = compareOption(x.breweryInfo.name, y.breweryInfo.name)
            case Sorting.byBreweryDec =>
              value = compareOption(y.breweryInfo.name, x.breweryInfo.name)
          }
        }
      }
      value
    }
  }

  def sort(seq: Seq[Beer], sortings: Seq[Sorting]): Seq[Beer] = {
    seq.sortBy(identity)(new BeerOrdering(sortings))
  }

  implicit class SortingExtensions(s: Sorting) {

    def toHumanReadable: String = {
      s match {
        case Sorting.byName    => "^ Название ^"
        case Sorting.byNameDec => "v Название v"

        case Sorting.byStyle    => "^ Стиль ^"
        case Sorting.byStyleDec => "v Стиль v"

        case Sorting.byRating    => "^ Рейтинг ^"
        case Sorting.byRatingDec => "v Рейтинг v"

        case Sorting.byPriceForMl    => "^ Цена ^"
        case Sorting.byPriceForMlDec => "^ Цена ^"

        case Sorting.byBrewery    => "^ Пивоварня ^"
        case Sorting.byBreweryDec => "v Пивоварня v"
      }
    }
  }
  @inline
  private def compareOption[A](x: Option[A], y: Option[A])(implicit ord: Ordering[A]): Int = {
    (x, y) match {
      case (Some(a), Some(b)) => ord.compare(a, b)
      case (Some(_), None)    => -1
      case (None, Some(_))    => 1
      case _                  => 0
    }
  }

  @inline
  private def getPriceForMl(b: Beer) = {
    val priceForMl = for {
      price <- b.price.map(_._2)
      volume <- b.draftType.flatMap {
                 case Sorting.mlRegex(ml) => Try(ml.toFloat).toOption
                 case Sorting.clRegex(cl) => Try(cl.toFloat * 10).toOption
                 case _                   => Option.empty[Float]
               }
    } yield price / volume

    priceForMl
  }

}
