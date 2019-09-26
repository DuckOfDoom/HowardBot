import cats.syntax.option._
import org.duckofdoom.howardbot.bot.utils.Sorting
import org.scalatest.{FlatSpec, Matchers}

class SortingSpec extends FlatSpec with Matchers {

  "Sorting" should "correctly calculate price per ML" in {
    Sorting.getPriceForMl(Utils.mkBeer("300 ml Bottle", 300)) should be(1.some)
    Sorting.getPriceForMl(Utils.mkBeer("100 ML", 1)) should be(0.01f.some)
    Sorting.getPriceForMl(Utils.mkBeer("150ml Can", 300)) should be(2.some)
    Sorting.getPriceForMl(Utils.mkBeer("100ML Zebra", 50)) should be(0.5.some)
  }

  it should "correctly calculate price per ML (when in CL)" in {
    Sorting.getPriceForMl(Utils.mkBeer("300 ml", 300)) should be(1.some)
    Sorting.getPriceForMl(Utils.mkBeer("100 ML", 1)) should be(0.01f.some)
    Sorting.getPriceForMl(Utils.mkBeer("150ml", 300)) should be(2.some)
    Sorting.getPriceForMl(Utils.mkBeer("100ML", 50)) should be(0.5.some)
  }

  it should "correctly sort stuff without something" in {

    val beers = Seq(
      Utils.mkBeer(1, None, None),
      Utils.mkBeer(2, "300 ml".some, 100f.some)
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
      .sort(Utils.mkBeers(), Seq(Sorting.byName))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 4, 3, 1)
  }
  
  it should "sort by name reversed" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byNameDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 4, 2)
  }

  it should "sort by style" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byStyle))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 4, 2)
  }
  
  it should "sort by style reversed" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byStyleDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 3, 4, 1)
  }

  it should "sort by rating" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byRating))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 2, 4, 1)
  }
  
  it should "sort by rating reversed" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byRatingDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 2, 4, 3)
  }

  it should "sort by price per ml" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byPriceForMl))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 2, 4)
  }
  
  it should "sort by price per ml reversed" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byPriceForMlDec))
      .map(_.id) should contain theSameElementsInOrderAs Seq(2, 4, 1, 3)
  }

  it should "sort by style and name" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byStyle, Sorting.byName))
      .map(_.id) should contain theSameElementsInOrderAs Seq(1, 4, 3, 2)
  }

  it should "sort by rating and style" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byRating, Sorting.byStyle))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 4, 2, 1)
  }

  it should "sort by price and brewery" in {
    Sorting
      .sort(Utils.mkBeers(), Seq(Sorting.byPriceForMl, Sorting.byBrewery))
      .map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 4, 2)
  }

}
