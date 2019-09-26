import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import cats.syntax.option._

object Utils {
  
// TODO: Yuck, use named arguments
  def mkBeer(
      id: Int,
      name: String,
      rating: (Float, Float),
      style: String,
      breweryName: String,
      draftType: String,
      price: Float
  ): Beer = {
    Beer(
      id,
      isInStock = true,
      LocalDateTime.MIN,
      LocalDateTime.MIN,
      name.some,
      None,
      rating.some,
      None,
      None,
      None,
      None,
      BreweryInfo(breweryName.some, None, None),
      style.some,
      draftType.some,
      ("", price).some,
      None
    )
  }

  def mkBeer(
      draftType: String,
      price: Float
  ): Beer = {

    mkBeer(0, draftType.some, price.some)
  }

  def mkBeer(id: Int, draftType: Option[String], price: Option[Float]): Beer = {
    Beer(
      id,
      isInStock = true,
      LocalDateTime.MIN,
      LocalDateTime.MIN,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      BreweryInfo(None, None, None),
      None,
      draftType,
      if (price.isEmpty) Option.empty[(String, Float)] else ("", price.get).some,
      None
    )
  }

  def mkBeers(): Seq[Beer] = {
    Seq(
      mkBeer(1, "Z", (5f, 4f), "B", "J", "300ml", 300),
      mkBeer(2, "F", (4f, 4f), "T", "X", "50cl", 5000),
      mkBeer(3, "O", (1f, 4f), "M", "R", "400 ml", 200),
      mkBeer(4, "K", (4f, 4f), "M", "J", "50cl", 5000)
    )
  }

}
