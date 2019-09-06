import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import org.scalatest.{FunSuite, Matchers}
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.Sorting

class SortingTests extends FunSuite with Matchers {

  private def mkBeer(id:Int, name:String, rating:(Float, Float), style:String, breweryName: String, draftType : String, price:Float) : Beer = {
    Beer(
      id,
      None,
      name.some,
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

  def mkBeers(): Seq[Beer] = {
    Seq(
      mkBeer(1, "Z", (5f, 4f), "B", "J", "300ml", 300),
      mkBeer(2, "F", (4f, 4f), "T", "X", "50cl", 5000),
      mkBeer(3, "O", (1f, 4f), "M", "R", "400 ml", 200),
      mkBeer(4, "K", (4f, 4f), "M", "J", "50cl", 5000)
    )
  }

  test("Sort by name") {
   Sorting.sort(mkBeers(), Sorting.byName).map(_.id) should contain theSameElementsInOrderAs Seq(2, 4, 3, 1)
  }
  
  test("Sort by style") {
    Sorting.sort(mkBeers(), Sorting.byStyle).map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 4, 2)
  }
  
  test("Sort by rating") {
    Sorting.sort(mkBeers(), Sorting.byRating).map(_.id) should contain theSameElementsInOrderAs Seq(3, 2, 4, 1)
  }
  
  test("Sort by price per ml") {
    Sorting.sort(mkBeers(), Sorting.byPriceForMl).map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 2, 4)
  }
  
  test("Sort by style and name") {
    Sorting.sort(mkBeers(), Sorting.byStyle, Sorting.byName).map(_.id) should contain theSameElementsInOrderAs Seq(1, 4, 3, 2)
  }
  
  test("Sort by rating and style") {
    Sorting.sort(mkBeers(), Sorting.byRating, Sorting.byStyle).map(_.id) should contain theSameElementsInOrderAs Seq(3, 4, 2, 1)
  }
  
  test("Sort by price and brewery") {
    Sorting.sort(mkBeers(), Sorting.byPriceForMl, Sorting.byBrewery).map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 4, 2)
  }
}
