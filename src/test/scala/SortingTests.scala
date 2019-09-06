import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import org.scalatest.{FunSuite, Matchers}
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.Sorting

class SortingTests extends FunSuite with Matchers {

  private def mkBeers(): Seq[Beer] = {
    scala.util.Random.shuffle(
      Seq(
        Beer(
          1,
          None,
          "Z".some,      // name
          (5f, 4f).some, // rating
          None,
          None,
          None,
          None,
          BreweryInfo("B".some, None, None),
          "J".some,
          "300ml".some,
          ("", 300f).some, // 300/500 = 1
          None
        ),
        Beer(
          2,
          None,
          "F".some,      // name
          (4f, 4f).some, // rating
          None,
          None,
          None,
          None,
          BreweryInfo("T".some, None, None),
          "X".some,
          "50cl".some,
          ("", 5000f).some, // 5000/500 = 10 
          None
        ),
        Beer(
          3,
          None,
          "O".some,      // name
          (1f, 4f).some, // rating
          None,
          None,
          None,
          None,
          BreweryInfo("M".some, None, None),
          "R".some,
          "400 ml".some,
          ("", 200f).some, // 200/400 = 0.5 
          None
        )
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
