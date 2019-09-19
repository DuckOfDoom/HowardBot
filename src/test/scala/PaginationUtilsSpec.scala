import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardButton
import org.duckofdoom.howardbot.bot.utils.Callback
import org.duckofdoom.howardbot.utils.PaginationUtils
import org.scalatest.{FlatSpec, Matchers}

class PaginationUtilsSpec extends FlatSpec with Matchers {
  implicit val callbackType: Callback.Type.Value = Callback.Type.Menu

  private def result(currentPage: Int, itemsCount: Int)(implicit itemsPerPage: Int) = {
    PaginationUtils.mkButtonsForPaginatedQuery(currentPage, itemsPerPage, itemsCount, page => Callback.mkMenuCallbackData(page.some, newMessage = false))
  }

  implicit val itemsPerPage: Int = 8
  implicit val payload: String   = ""

  "Pagination Utils" should "generate buttons correctly when there are more pages than buttons." in {

    val itemsCount      = 50
    val totalPages: Int = (itemsCount / itemsPerPage) + 1

    var curr = 1

    List((1, 3), (3, 4)) should contain theSameElementsAs List((1, 3), (3, 4))

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkCurr(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), Callback.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), Callback.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), Callback.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), Callback.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = 3
    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(3), Callback.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), Callback.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    for (i <- 4 to (totalPages - 4)) {
      curr = i

      result(curr, itemsCount) should contain theSameElementsAs List(
        InlineKeyboardButton(PaginationUtils.mkFirst(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
               Callback.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkCurr(curr), Callback.mkMenuCallbackData(curr.some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
               Callback.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
               Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
      )
    }

    curr = totalPages - 3

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
             Callback.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), Callback.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
             Callback.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages - 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
             Callback.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), Callback.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
             Callback.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages - 1

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 2),
             Callback.mkMenuCallbackData((curr - 2).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 1),
             Callback.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), Callback.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 3),
             Callback.mkMenuCallbackData((curr - 3).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 2),
             Callback.mkMenuCallbackData((curr - 2).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 1),
             Callback.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(totalPages),
             Callback.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )
  }

  it should "generate buttons correctly when there are less than 5 pages" in {

    var itemsCount = 14

    result(1, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkCurr(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
    )

    result(2, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
    )

    // Edge case for exact items count
    itemsCount = itemsPerPage * 5

    result(4, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), Callback.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), Callback.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), Callback.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(4), Callback.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(5), Callback.mkMenuCallbackData(5.some, newMessage = false).some),
    )
  }

  it should "generate buttons correctly when there are no pages" in {

    val itemsCount = 8

    for (i <- 1 to itemsCount) {
      result(i, itemsCount) shouldBe empty
    }
  }
}
