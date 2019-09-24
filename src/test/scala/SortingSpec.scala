import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import org.duckofdoom.howardbot.bot.utils.Sorting
import org.scalatest.{FlatSpec, Matchers}

class SortingSpec extends FlatSpec with Matchers {

  "Sorting" should "correctly calculate price per ML" in {
    Sorting.getPriceForMl(mkBeer("300 ml Bottle", 300)) should be(1.some)
    Sorting.getPriceForMl(mkBeer("100 ML", 1)) should be(0.01f.some)
    Sorting.getPriceForMl(mkBeer("150ml Can", 300)) should be(2.some)
    Sorting.getPriceForMl(mkBeer("100ML Zebra", 50)) should be(0.5.some)
  }

  it should "correctly calculate price per ML (when in CL)" in {
    Sorting.getPriceForMl(mkBeer("300 ml", 300)) should be(1.some)
    Sorting.getPriceForMl(mkBeer("100 ML", 1)) should be(0.01f.some)
    Sorting.getPriceForMl(mkBeer("150ml", 300)) should be(2.some)
    Sorting.getPriceForMl(mkBeer("100ML", 50)) should be(0.5.some)
  }

  it should "correctly sort stuff without something" in {

    val beers = Seq(
      mkBeer(1, None, None),
      mkBeer(2, "300 ml".some, 100f.some)
    )

    Sorting.sort(
      beers,
      Seq(Sorting.byPriceForMl)
    ) map (_.id) should contain theSameElementsInOrderAs Seq(2, 1)

    Sorting.sort(
      beers,
      Seq(Sorting.byPriceForMlDec)
    ) map (_.id) should contain theSameElementsInOrderAs Seq(2, 1)
  }

  it should "sort by name" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byName))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 4, 3, 1)
  }
  
  it should "sort by name reversed" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byNameDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 4, 2)
  }

  it should "sort by style" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byStyle))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 4, 2)
  }
  
  it should "sort by style reversed" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byStyleDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 3, 4, 1)
  }

  it should "sort by rating" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byRating))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 2, 4, 1)
  }
  
  it should "sort by rating reversed" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byRatingDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 2, 4, 3)
  }

  it should "sort by price per ml" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byPriceForMl))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 2, 4)
  }
  
  it should "sort by price per ml reversed" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byPriceForMlDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 4, 1, 3)
  }

  it should "sort by style and name" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byStyle, Sorting.byName))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 4, 3, 2)
  }

  it should "sort by rating and style" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byRating, Sorting.byStyle))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 4, 2, 1)
  }

  it should "sort by price and brewery" in {
    Sorting
      .sort(mkBeers(), Seq(Sorting.byPriceForMl, Sorting.byBrewery))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 4, 2)
  }

  private def mkBeer(
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

  private def mkBeer(
      draftType: String,
      price: Float
  ): Beer = {

    mkBeer(0, draftType.some, price.some)
  }

  private def mkBeer(id: Int, draftType: Option[String], price: Option[Float]): Beer = {
    Beer(
      id,
      isInStock = true,
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
