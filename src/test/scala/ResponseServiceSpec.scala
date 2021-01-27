import com.bot4s.telegram.models.InlineKeyboardMarkup
import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import org.duckofdoom.howardbot.bot.services.{ItemsProvider, KeyboardService, ResponseService, ResponseServiceImpl}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

// TODO: Finish these tests =_=
class ResponseServiceSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  private def fixture: Object {
    var service: ResponseService
  } = new {

    implicit val itemsProvider: ItemsProvider    = createItemsProvider()
    implicit val config: Config                  = createConfig()
    implicit val keyboardHelper: KeyboardService = createKeyboardHelper()

    var service: ResponseService = new ResponseServiceImpl(10, 10, itemsProvider, keyboardHelper)
  }

  val defaultKeyboard: InlineKeyboardMarkup = InlineKeyboardMarkup(Seq())

  "ResponseService" should "return correct response for empty search results" in {
    val service = fixture.service
    service.mkEmptySearchResultsResponse("NON-EXISTENT BEER") should be(
      ("По запросу 'NON-EXISTENT BEER' ничего не найдено :(", defaultKeyboard)
    )
  }

  private def createItemsProvider(): ItemsProvider = {
    val m = mock[ItemsProvider]

    m.availableBeers _ expects () anyNumberOfTimes () onCall { () =>
      List(
        Beer(1, isInStock = true, name = "beer1".some, style = "style1".some),
        Beer(2, isInStock = true, name = "beer2".some, style = "style1".some),
        Beer(3, isInStock = true, name = "beer3".some, style = "style2".some)
      )
    }

    m
  }

  private def createKeyboardHelper(): KeyboardService = {
    val m = mock[KeyboardService]
    m.mkDefaultButtons _ expects * anyNumberOfTimes () onCall { _: Boolean =>
      defaultKeyboard
    }
    m
  }

  private def createConfig(): Config = {
    val m = stub[Config]
    m
  }
}
