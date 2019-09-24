import com.bot4s.telegram.models.InlineKeyboardMarkup
import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.bot.services.{KeyboardHelper, ResponseHelper, ResponseService, ResponseServiceImpl}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

// TODO: Finish these tests =_=
class ResponseServiceSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  def fixture = new {

    implicit val itemsProvider: ItemsProvider   = createItemsProvider()
    implicit val config: Config                 = createConfig()
    implicit val responseHelper: ResponseHelper = createResponseHelper()
    implicit val keyboardHelper: KeyboardHelper = createKeyboardHelper()

    var service : ResponseService = new ResponseServiceImpl()
  }
  
  val defaultKeyboard = InlineKeyboardMarkup(Seq())

  "ResponseService" should "return correct response for empty search results" in {
    val service = fixture.service
    service.mkSearchBeerByNameResponse("NON-EXISTENT BEER", 0, Seq()) should be (("NON-EXISTENT BEER is missing", defaultKeyboard))
    service.mkSearchBeerByStyleResponse("NON-EXISTENT STYLE", 0, Seq()) should be (("NON-EXISTENT STYLE is missing", defaultKeyboard))
  }
//  
//  it should "return correct response for non-empty search results" in {
//    val service = fixture.service
//    service.mkSearchBeerByNameResponse("beer3", 0, Seq()) should be (("this is beer3 response", defaultKeyboard))
//  }

  private def createItemsProvider(): ItemsProvider = {
    val m = mock[ItemsProvider]

    m.beers _ expects () anyNumberOfTimes() onCall { () =>
      List(
        Beer(1, name = "beer1".some, style = "style1".some),
        Beer(2, name = "beer2".some, style = "style1".some),
        Beer(3, name = "beer3".some, style = "style2".some),
      )
    }  
    
    m
  }

  private def createResponseHelper(): ResponseHelper = {
    val m = mock[ResponseHelper]
    m.mkEmptySeachResultsResponse _ expects * anyNumberOfTimes() onCall { arg : String => arg + " is missing"}
    
//    (m.mkPaginatedResponse _ _ _ _) expects (* * * * *) anyNumberOfTimes() onCall { arg : String => s"this is $arg response"}
    m
  }

  private def createKeyboardHelper(): KeyboardHelper = {
    val m = mock[KeyboardHelper]
    
    m.mkDefaultButtons _ expects * anyNumberOfTimes() onCall { _:Boolean => defaultKeyboard }
    
    m    
  }

  private def createConfig(): Config = {
    val m = stub[Config]
    
    m
  }
}
