package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot
import org.duckofdoom.howardbot.bot.data.Beer

import scala.util.Try
import scala.util.matching.Regex

object Sorting extends Enumeration {

  type Sorting = Value

  val byName: bot.Sorting.Value       = Value("n")
  val byStyle: bot.Sorting.Value      = Value("s")
  val byRating: bot.Sorting.Value     = Value("r")
  val byPriceForMl: bot.Sorting.Value = Value("p")
  val byBrewery: bot.Sorting.Value    = Value("b")

  val mlRegex: Regex = "(\\d+)\\s*ml".r
  val clRegex: Regex = "(\\d+)\\s*cl".r

  import util.control.Breaks._

  class BeerOrdering(sortings: Sorting*) extends Ordering[Beer] {

    override def compare(x: Beer, y: Beer): Int = {
      var value = 0

      breakable {
        for (s <- sortings) {
          // Only compare next if values are equal
          if (value != 0)
            break

          s match {
            case Sorting.byName   => value = compareOption(x.name, y.name)
            case Sorting.byStyle  => value = compareOption(x.style, y.style)
            case Sorting.byRating => value = compareOption(x.rating.map(_._1), y.rating.map(_._1))
            case Sorting.byPriceForMl =>
              def getPriceForMl(b: Beer) = {
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

              value = compareOption(getPriceForMl(x), getPriceForMl(y));

            case Sorting.byBrewery => value = compareOption(x.breweryInfo.name, y.breweryInfo.name)
          }
        }
      }
      value
    }
  }

  @inline
  private def compareOption[A](a: Option[A], b: Option[A])(implicit ord: Ordering[A]): Int = {
    (a, b) match {
      case (Some(a1), Some(a2)) => ord.compare(a1, a2)
      case (Some(_), None)      => -1
      case (None, Some(_))      => 1
    }
  }

  def sort(seq: Seq[Beer], sortings: Sorting*): Seq[Beer] = {
    seq.sortBy(identity)(new BeerOrdering(sortings: _*))

//    @inline
//    def compareOption[A](a: Option[A], b: Option[A])(implicit ord: Ordering[A]): Boolean = {
//      (a, b) match {
//        case (Some(a1), Some(a2)) => ord.lt(a1, a2)
//        case (Some(_), None) => false
//        case (None, Some(_)) => true
//      }
//    }
//
//    val sortingFunc: (Beer, Beer) => Boolean = (b1, b2) => {
//      var lt = true
//
//      for (s <- sortings) {
//
//        s match {
//          case Sorting.byName => lt = lt & compareOption(b1.name, b2.name)
//          case Sorting.byStyle => lt = lt &  compareOption(b1.style, b2.style)
//          case Sorting.byRating => lt = lt & compareOption(b1.rating.map(_._1), b2.rating.map(_._1))
//          case Sorting.byPriceForMl =>
//            def getPriceForMl(b:Beer) = {
//              val priceForMl = for {
//                price <- b.price.map(_._2)
//                volume <- b.draftType.flatMap {
//                  case Sorting.mlRegex(ml) => Try(ml.toFloat).toOption
//                  case Sorting.clRegex(cl) => Try(cl.toFloat * 10).toOption
//                  case _ => Option.empty[Float]
//                }
//              } yield price / volume
//
//              priceForMl
//            }
//
//            lt = lt & compareOption(getPriceForMl(b1), getPriceForMl(b2));
//
//          case Sorting.byBrewery => lt = lt & compareOption(b1.breweryInfo.name, b2.breweryInfo.name)
//        }
//      }
//
//      lt
//    }
//
//    seq.sortWith(sortingFunc)
  }
}
