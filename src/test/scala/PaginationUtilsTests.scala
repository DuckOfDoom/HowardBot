import org.duckofdoom.howardbot.bot.CallbackUtils
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
import org.duckofdoom.howardbot.utils.{Button, PaginationUtils}
import org.scalatest.{FunSuite, Matchers}

import cats.syntax.option._

class PaginationUtilsTests extends FunSuite with Matchers {
  implicit val callbackType: CallbackUtils.CallbackType.Value = CallbackType.Menu

  private def result(currentPage: Int, itemsCount: Int)(implicit itemsPerPage: Int) = {
    PaginationUtils.mkButtonsForPaginatedQuery(currentPage, itemsPerPage, itemsCount, None)
  }

  implicit val itemsPerPage: Int = 8
  implicit val payload: String   = ""

  test("Buttons generation. More pages than buttons.") {

    val itemsCount      = 50
    val totalPages: Int = (itemsCount / itemsPerPage) + 1

    var curr = 1

    List((1, 3), (3, 4)) should contain theSameElementsAs List((1, 3), (3, 4))

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkCurr(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallbackData(2.some, None)),
      Button(PaginationUtils.mkNormal(3), PaginationUtils.mkCallbackData(3.some, None)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallbackData(4.some, None)),
      Button(PaginationUtils.mkLast(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    curr = 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkCurr(2), PaginationUtils.mkCallbackData(2.some, None)),
      Button(PaginationUtils.mkNormal(3), PaginationUtils.mkCallbackData(3.some, None)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallbackData(4.some, None)),
      Button(PaginationUtils.mkLast(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    curr = 3
    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallbackData(2.some, None)),
      Button(PaginationUtils.mkCurr(3), PaginationUtils.mkCallbackData(3.some, None)),
      Button(PaginationUtils.mkNext(4), PaginationUtils.mkCallbackData(4.some, None)),
      Button(PaginationUtils.mkLast(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    for (i <- 4 to (totalPages - 4)) {
      curr = i

      result(curr, itemsCount) should contain theSameElementsAs List(
        Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, None)),
        Button(PaginationUtils.mkPrev(curr - 1),
               PaginationUtils.mkCallbackData((curr - 1).some, None)),
        Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallbackData(curr.some, None)),
        Button(PaginationUtils.mkNext(curr + 1),
               PaginationUtils.mkCallbackData((curr + 1).some, None)),
        Button(PaginationUtils.mkLast(totalPages),
               PaginationUtils.mkCallbackData(totalPages.some, None))
      )
    }

    curr = totalPages - 3

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkPrev(curr - 1),
             PaginationUtils.mkCallbackData((curr - 1).some, None)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallbackData(curr.some, None)),
      Button(PaginationUtils.mkNext(curr + 1),
             PaginationUtils.mkCallbackData((curr + 1).some, None)),
      Button(PaginationUtils.mkLast(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    curr = totalPages - 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkPrev(curr - 1),
             PaginationUtils.mkCallbackData((curr - 1).some, None)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallbackData(curr.some, None)),
      Button(PaginationUtils.mkNext(curr + 1),
             PaginationUtils.mkCallbackData((curr + 1).some, None)),
      Button(PaginationUtils.mkLast(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    curr = totalPages - 1

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkPrev(curr - 2),
             PaginationUtils.mkCallbackData((curr - 2).some, None)),
      Button(PaginationUtils.mkNormal(curr - 1),
             PaginationUtils.mkCallbackData((curr - 1).some, None)),
      Button(PaginationUtils.mkCurr(curr), PaginationUtils.mkCallbackData(curr.some, None)),
      Button(PaginationUtils.mkNormal(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )

    curr = totalPages

    result(curr, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkFirst(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkPrev(curr - 3),
             PaginationUtils.mkCallbackData((curr - 3).some, None)),
      Button(PaginationUtils.mkNormal(curr - 2),
             PaginationUtils.mkCallbackData((curr - 2).some, None)),
      Button(PaginationUtils.mkNormal(curr - 1),
             PaginationUtils.mkCallbackData((curr - 1).some, None)),
      Button(PaginationUtils.mkCurr(totalPages),
             PaginationUtils.mkCallbackData(totalPages.some, None))
    )
  }

  test("Buttons generation. Less than 5 pages.") {

    var itemsCount = 14

    result(1, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkCurr(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallbackData(2.some, None)),
    )

    result(2, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkCurr(2), PaginationUtils.mkCallbackData(2.some, None)),
    )

    // Edge case for exact items count
    itemsCount = itemsPerPage * 5

    result(4, itemsCount) should contain theSameElementsAs List(
      Button(PaginationUtils.mkNormal(1), PaginationUtils.mkCallbackData(1.some, None)),
      Button(PaginationUtils.mkNormal(2), PaginationUtils.mkCallbackData(2.some, None)),
      Button(PaginationUtils.mkNormal(3), PaginationUtils.mkCallbackData(3.some, None)),
      Button(PaginationUtils.mkCurr(4), PaginationUtils.mkCallbackData(4.some, None)),
      Button(PaginationUtils.mkNormal(5), PaginationUtils.mkCallbackData(5.some, None)),
    )
  }

  test("Buttons generation. No pages.") {

    val itemsCount = 8

    for (i <- 1 to itemsCount) {
      result(i, itemsCount) shouldBe empty
    }
  }
}
