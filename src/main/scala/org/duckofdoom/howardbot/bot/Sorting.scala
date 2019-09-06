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

  def sort(seq: Seq[Beer], sortings: Sorting*): Seq[Beer] = {
    
    @inline
    def compareOption[A](a: Option[A], b: Option[A])(implicit ord: Ordering[A]): Boolean = {
      (a, b) match {
        case (Some(a1), Some(a2)) => ord.lt(a1, a2) 
        case (Some(_), None) => false
        case (None, Some(_)) => true
      }
    }

    val sortingFunc: (Beer, Beer) => Boolean = (b1, b2) => {
      var lt = true
      var prevIsEqual = 
      
      for (s <- sortings) {
        
        s match {
          case Sorting.byName => lt = lt & compareOption(b1.name, b2.name)
          case Sorting.byStyle => lt = lt &  compareOption(b1.style, b2.style)
          case Sorting.byRating => lt = lt & compareOption(b1.rating.map(_._1), b2.rating.map(_._1))
          case Sorting.byPriceForMl =>
            def getPriceForMl(b:Beer) = {
              val priceForMl = for {
                price <- b.price.map(_._2)
                volume <- b.draftType.flatMap {
                  case Sorting.mlRegex(ml) => Try(ml.toFloat).toOption
                  case Sorting.clRegex(cl) => Try(cl.toFloat * 10).toOption
                  case _ => Option.empty[Float]
                }
              } yield price / volume

              priceForMl
            }
            
            lt = lt & compareOption(getPriceForMl(b1), getPriceForMl(b2));
            
          case Sorting.byBrewery => lt = lt & compareOption(b1.breweryInfo.name, b2.breweryInfo.name)
        }
      }
      
      lt 
    }
    
    seq.sortWith(sortingFunc)
  }
}
