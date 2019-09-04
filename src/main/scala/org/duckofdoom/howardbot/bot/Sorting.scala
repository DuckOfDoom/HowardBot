package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot
import org.duckofdoom.howardbot.bot.data.{Beer, Item}

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

  def sort(seq: Seq[Beer], sortings: Seq[Sorting]): Seq[Beer] = {
    
    var sorted = seq

    for (s <- sortings) {
      s match {
        case Sorting.byName   => sorted = sorted.sortBy(_.name)
        case Sorting.byStyle  => sorted = sorted.sortBy(_.style.getOrElse(""))
        case Sorting.byRating => sorted = sorted.sortBy[Float](_.rating.map(_._1).getOrElse(0f))
        case Sorting.byPriceForMl =>
          sorted = sorted.sortBy[Float](b => {

            val priceForMl = for {
              price <- b.price.map(_._2)
              volume <- b.draftType.flatMap {
                         case Sorting.mlRegex(ml) => Try(ml.toFloat).toOption
                         case Sorting.clRegex(cl) => Try(cl.toFloat * 10).toOption
                         case _                   => Option.empty[Float]
                       }
            } yield price / volume

            priceForMl.getOrElse(0f)
          })
        case Sorting.byBrewery => sorted = sorted.sortBy(_.breweryInfo.name.getOrElse(""))
      }
    }
    
    sorted
  }
}
