package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot
import org.duckofdoom.howardbot.bot.data.Beer

import scala.util.Try
import scala.util.matching.Regex

import util.control.Breaks._

object Sorting extends Enumeration {

  type Sorting = Value

  val byName: bot.Sorting.Value    = Value("n")
  val byNameDec: bot.Sorting.Value = Value("n")

  val byStyle: bot.Sorting.Value    = Value("s")
  val byStyleDec: bot.Sorting.Value = Value("s")

  val byRating: bot.Sorting.Value    = Value("r")
  val byRatingDec: bot.Sorting.Value = Value("r")

  val byPriceForMl: bot.Sorting.Value    = Value("p")
  val byPriceForMlDec: bot.Sorting.Value = Value("p")

  val byBrewery: bot.Sorting.Value    = Value("b")
  val byBreweryDec: bot.Sorting.Value = Value("b")

  val mlRegex: Regex = """(\d+)\s*(ml|ML).*""".r
  val clRegex: Regex = """(\d+)\s*(cl|CL).*""".r

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
            case Sorting.byName    => value = compareOption(x.name, y.name, reversed = false)
            case Sorting.byNameDec => value = compareOption(x.name, y.name, reversed = true)

            case Sorting.byStyle    => value = compareOption(x.style, y.style, reversed = false)
            case Sorting.byStyleDec => value = compareOption(x.style, y.style, reversed = true)

            case Sorting.byRating =>
              value = compareOption(x.rating.map(_._1), y.rating.map(_._1), reversed = false)
            case Sorting.byRatingDec =>
              value = compareOption(x.rating.map(_._1), y.rating.map(_._1), reversed = true)

            case Sorting.byPriceForMl =>
              value = compareOption(getPriceForMl(x), getPriceForMl(y), reversed = false)
            case Sorting.byPriceForMlDec =>
              value = compareOption(getPriceForMl(x), getPriceForMl(y), reversed = true)

            case Sorting.byBrewery =>
              value = compareOption(x.breweryInfo.name, y.breweryInfo.name, reversed = false)
            case Sorting.byBreweryDec =>
              value = compareOption(x.breweryInfo.name, y.breweryInfo.name, reversed = true)
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
        case Sorting.byPriceForMlDec => "v Цена v"

        case Sorting.byBrewery    => "^ Пивоварня ^"
        case Sorting.byBreweryDec => "v Пивоварня v"
      }
    }
  }

  @inline
  def getPriceForMl(b: Beer): Option[Float] = {
    val priceForMl = for {
      price <- b.price.map(_._2)
      volume <- b.draftType.flatMap {
                 case Sorting.mlRegex(ml, _) => Try(ml.toFloat).toOption
                 case Sorting.clRegex(cl, _) => Try(cl.toFloat * 10).toOption
                 case _                      => Option.empty[Float]
               }
    } yield price / volume

    priceForMl
  }

  @inline
  private def compareOption[A](x: Option[A], y: Option[A], reversed: Boolean)(
      implicit ord: Ordering[A]
  ): Int = {
    (x, y) match {
      case (Some(a), Some(b)) => ord.compare(a, b) * (if (reversed) -1 else 1)
      case (Some(_), None)    => -1
      case (None, Some(_))    => 1
      case _                  => 0
    }
  }

}
