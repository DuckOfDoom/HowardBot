import org.duckofdoom.howardbot.bot.ResponseService
import org.duckofdoom.howardbot.utils.PaginationUtils
import org.scalatest.FunSuite

class ResponseServiceTest extends FunSuite {
  private def result(currentPage: Int, itemsCount: Int)(implicit itemsPerPage: Int) = {
    ResponseService.mkButtonsForPaginatedQuery(currentPage, itemsPerPage, itemsCount)
  }

  implicit val itemsPerPage: Int = 8

  test("Buttons generation. A lot of pages.") {

    val itemsCount      = 20
    val totalPages: Int = math.ceil(itemsCount / 8).toInt
    

    assert(
      result(1, itemsCount) == List(
        (PaginationUtils.mkCurr(1), "1"),
        (PaginationUtils.mkNormal(2), "2"),
        (PaginationUtils.mkNormal(3), "3"),
        (PaginationUtils.mkNext(4), "4"),
        (PaginationUtils.mkLast(totalPages), totalPages)
      )
    )

    assert(
      result(2, itemsCount) == List(
        (PaginationUtils.mkNormal(1), "1"),
        (PaginationUtils.mkCurr(2), "2"),
        (PaginationUtils.mkNormal(3), "3"),
        (PaginationUtils.mkNext(4), "4"),
        (PaginationUtils.mkLast(totalPages), totalPages)
      )
    )

    assert(
      result(3, itemsCount) == List(
        (PaginationUtils.mkPrev(1), "1"),
        (PaginationUtils.mkNormal(2), "2"),
        (PaginationUtils.mkCurr(3), "3"),
        (PaginationUtils.mkNext(4), "4"),
        (PaginationUtils.mkLast(totalPages), totalPages)
      )
    )

    assert(
      result(4, itemsCount) == List(
        (PaginationUtils.mkFirst(1), "1"),
        (PaginationUtils.mkPrev(2), "2"),
        (PaginationUtils.mkNormal(4), "4"),
        (PaginationUtils.mkNext(5), "5"),
        (PaginationUtils.mkLast(totalPages), totalPages)
      )
    )

    for (i <- 5 to totalPages) {
      assert(
        result(i, itemsCount) == List(
          (PaginationUtils.mkFirst(1), "1"),
          (PaginationUtils.mkPrev(i - 1), (i - 1).toString),
          (PaginationUtils.mkNormal(i), i.toString),
          (PaginationUtils.mkNext(i + 1), (i + 1).toString),
          (PaginationUtils.mkLast(totalPages), totalPages)
        )
      )
    }
    
    // TODO: ADD tests for end of the list
  }
}
