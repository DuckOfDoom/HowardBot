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

  private def mkBeers(): Seq[Beer] = {
    scala.util.Random.shuffle(
      Seq(
        mkBeer(1, "Z", (5f, 4f), "B", "J", "300ml", 300),
        mkBeer(2, "F", (4f, 4f), "T", "X", "50cl", 5000),
        mkBeer(3, "O", (1f, 4f), "M","R", "400 ml", 200)
      )
    )
  }

  test("Sort by name") {
   Sorting.sort(mkBeers(), Sorting.byName).map(_.id) should contain theSameElementsInOrderAs Seq(2, 3, 1)
  }
  
  test("Sort by name and style") {
    Sorting.sort(mkBeers(), Sorting.byName, Sorting.byStyle).map(_.id) should contain theSameElementsInOrderAs Seq(2, 3, 1)
  }
  
  test("Sort by style") {
    Sorting.sort(mkBeers(), Sorting.byStyle).map(_.id) should contain theSameElementsInOrderAs Seq(1, 3, 2)
  }
  
  test("Sort by rating") {
    Sorting.sort(mkBeers(), Sorting.byRating).map(_.id) should contain theSameElementsInOrderAs Seq(3, 2, 1)
  }
  
  test("Sort by price per ml") {
    Sorting.sort(mkBeers(), Sorting.byPriceForMl).map(_.id) should contain theSameElementsInOrderAs Seq(3, 1, 2)
  }
}
