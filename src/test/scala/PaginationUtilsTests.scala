import cats.syntax.option._
import com.bot4s.telegram.models.InlineKeyboardButton
import org.duckofdoom.howardbot.bot.CallbackUtils
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
import org.duckofdoom.howardbot.utils.PaginationUtils
import org.scalatest.{FunSuite, Matchers}

class PaginationUtilsTests extends FunSuite with Matchers {
  implicit val callbackType: CallbackUtils.CallbackType.Value = CallbackType.Menu

  private def result(currentPage: Int, itemsCount: Int)(implicit itemsPerPage: Int) = {
    PaginationUtils.mkButtonsForPaginatedQuery(currentPage, itemsPerPage, itemsCount, page => CallbackUtils.mkMenuCallbackData(page.some, newMessage = false))
  }

  implicit val itemsPerPage: Int = 8
  implicit val payload: String   = ""

  test("Buttons generation. More pages than buttons.") {

    val itemsCount      = 50
    val totalPages: Int = (itemsCount / itemsPerPage) + 1

    var curr = 1

    List((1, 3), (3, 4)) should contain theSameElementsAs List((1, 3), (3, 4))

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkCurr(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), CallbackUtils.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), CallbackUtils.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), CallbackUtils.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), CallbackUtils.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = 3
    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(3), CallbackUtils.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(4), CallbackUtils.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    for (i <- 4 to (totalPages - 4)) {
      curr = i

      result(curr, itemsCount) should contain theSameElementsAs List(
        InlineKeyboardButton(PaginationUtils.mkFirst(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
               CallbackUtils.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkCurr(curr), CallbackUtils.mkMenuCallbackData(curr.some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
               CallbackUtils.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
        InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
               CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
      )
    }

    curr = totalPages - 3

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
             CallbackUtils.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), CallbackUtils.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
             CallbackUtils.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages - 2

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 1),
             CallbackUtils.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), CallbackUtils.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNext(curr + 1),
             CallbackUtils.mkMenuCallbackData((curr + 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkLast(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages - 1

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 2),
             CallbackUtils.mkMenuCallbackData((curr - 2).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 1),
             CallbackUtils.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(curr), CallbackUtils.mkMenuCallbackData(curr.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )

    curr = totalPages

    result(curr, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkFirst(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkPrev(curr - 3),
             CallbackUtils.mkMenuCallbackData((curr - 3).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 2),
             CallbackUtils.mkMenuCallbackData((curr - 2).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(curr - 1),
             CallbackUtils.mkMenuCallbackData((curr - 1).some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(totalPages),
             CallbackUtils.mkMenuCallbackData(totalPages.some, newMessage = false).some)
    )
  }

  test("Buttons generation. Less than 5 pages.") {

    var itemsCount = 14

    result(1, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkCurr(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
    )

    result(2, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
    )

    // Edge case for exact items count
    itemsCount = itemsPerPage * 5

    result(4, itemsCount) should contain theSameElementsAs List(
      InlineKeyboardButton(PaginationUtils.mkNormal(1), CallbackUtils.mkMenuCallbackData(1.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(2), CallbackUtils.mkMenuCallbackData(2.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(3), CallbackUtils.mkMenuCallbackData(3.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkCurr(4), CallbackUtils.mkMenuCallbackData(4.some, newMessage = false).some),
      InlineKeyboardButton(PaginationUtils.mkNormal(5), CallbackUtils.mkMenuCallbackData(5.some, newMessage = false).some),
    )
  }

  test("Buttons generation. No pages.") {

    val itemsCount = 8

    for (i <- 1 to itemsCount) {
      result(i, itemsCount) shouldBe empty
    }
  }
}
