import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import org.scalatest.{FunSuite, Matchers}

class PaginationUtilsTests extends FunSuite with Matchers {
  private def result(currentPage: Int, itemsCount: Int)(implicit itemsPerPage: Int) = {
    PaginationUtils.mkButtonsForPaginatedQuery(currentPage, itemsPerPage, itemsCount)
  }

  implicit val itemsPerPage: Int = 8

  test("Buttons generation. More pages than buttons.") {

    val itemsCount      = 50
    val totalPages: Int = (itemsCount / 8) + 1

    var curr = 1

    List((1, 3), (3, 4)) should contain theSameElementsAs List((1, 3), (3, 4))

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkCurr(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallback(2)),
      Button(PaginationUtils.mkNormal(3), PaginationUtils.mkCallback(3)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallback(4)),
      Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    curr = 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkCurr(2), PaginationUtils.mkCallback(2)),
      Button(PaginationUtils.mkNormal(3), PaginationUtils.mkCallback(3)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallback(4)),
      Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    curr = 3
    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallback(2)),
      Button(PaginationUtils.mkCurr(3), PaginationUtils.mkCallback(3)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallback(4)),
      Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    for (i <- 4 to (totalPages - 4)) {
      curr = i

      result(curr, itemsCount) should contain theSameElementsAs List(
        Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1)),
        Button(PaginationUtils.mkPrev(curr - 1), PaginationUtils.mkCallback(curr - 1)),
        Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallback(curr)),
        Button(PaginationUtils.mkNext(curr + 1), PaginationUtils.mkCallback(curr + 1)),
        Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
      )
    }

    curr = totalPages - 3

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkPrev(curr - 1), PaginationUtils.mkCallback(curr - 1)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallback(curr)),
      Button(PaginationUtils.mkNext(curr + 1), PaginationUtils.mkCallback(curr + 1)),
      Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    curr = totalPages - 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkPrev(curr - 1), PaginationUtils.mkCallback(curr - 1)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallback(curr)),
      Button(PaginationUtils.mkNext(curr + 1), PaginationUtils.mkCallback(curr + 1)),
      Button(PaginationUtils.mkLast(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    curr = totalPages - 1

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkPrev(curr - 2), PaginationUtils.mkCallback(curr - 2)),
      Button(PaginationUtils.mkNormal(curr - 1), PaginationUtils.mkCallback(curr - 1)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallback(curr)),
      Button(PaginationUtils.mkNormal(totalPages), PaginationUtils.mkCallback(totalPages))
    )

    curr = totalPages

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallback(1)),
      Button(PaginationUtils.mkPrev(curr - 3), PaginationUtils.mkCallback(curr - 3)),
      Button(PaginationUtils.mkNormal(curr - 2), PaginationUtils.mkCallback(curr - 2)),
      Button(PaginationUtils.mkNormal(curr - 1), PaginationUtils.mkCallback(curr - 1)),
      Button(PaginationUtils.mkCurr(totalPages), PaginationUtils.mkCallback(totalPages))
    )
  }

  test("Buttons generation. Pages == Buttons") {
    val itemsCount = 5

    for (i <- 1 to itemsCount) {

      result(i, itemsCount) should contain theSameElementsAs List(
        Button(if (i == 1) PaginationUtils.mkCurr(1) else PaginationUtils.mkNormal(1),
               PaginationUtils.mkCallback(1)),
        Button(if (i == 2) PaginationUtils.mkCurr(2) else PaginationUtils.mkNormal(2),
               PaginationUtils.mkCallback(2)),
        Button(if (i == 3) PaginationUtils.mkCurr(3) else PaginationUtils.mkNormal(3),
               PaginationUtils.mkCallback(3)),
        Button(if (i == 4) PaginationUtils.mkCurr(4) else PaginationUtils.mkNormal(4),
               PaginationUtils.mkCallback(4)),
        Button(if (i == 5) PaginationUtils.mkCurr(5) else PaginationUtils.mkNormal(5),
               PaginationUtils.mkCallback(5))
      )
    }
  }
  
  test("Buttons generation. Less pages than buttons.") {

    val itemsCount = 3

    for (i <- 1 to itemsCount) {

      result(i, itemsCount) should contain theSameElementsAs List(
        Button(if (i == 1) PaginationUtils.mkCurr(1) else PaginationUtils.mkNormal(1),
               PaginationUtils.mkCallback(1)),
        Button(if (i == 2) PaginationUtils.mkCurr(2) else PaginationUtils.mkNormal(2),
               PaginationUtils.mkCallback(2)),
        Button(if (i == 3) PaginationUtils.mkCurr(3) else PaginationUtils.mkNormal(3),
               PaginationUtils.mkCallback(3))
      )
    }
  }
  
}
