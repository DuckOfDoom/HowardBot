import com.bot4s.telegram.models.InlineKeyboardMarkup
import cats.syntax.option._
import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.{Beer, ItemsProvider}
import org.duckofdoom.howardbot.bot.services.{KeyboardHelper, ResponseHelper, ResponseServiceImpl}
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

class ResponseServiceSpec extends FlatSpec with Matchers with MockFactory {

  implicit val itemsProvider: ItemsProvider   = createItemsProvider()
  implicit val config: Config                 = createConfig()
  implicit val responseHelper: ResponseHelper = createResponseHelper()
  implicit val keyboardHelper: KeyboardHelper = createKeyboardHelper()
  
  val defaultKeyboard = InlineKeyboardMarkup(Seq())

  val service = new ResponseServiceImpl()

  "ResponseService" should "return correct response for empty search results" in {
    service.mkSearchBeerByNameResponse("NON-EXISTENT BEER", 0, Seq()) should be (("NON-EXISTENT BEER is missing", defaultKeyboard))
    service.mkSearchBeerByStyleResponse("NON-EXISTENT STYLE", 0, Seq()) should be (("NON-EXISTENT STYLE is missing", defaultKeyboard))
    itemsProvider.beers should have length (3)
  }
  
  it should "return correct response for non-empty search results" in {
//    service.mkSearchBeerByNameResponse("sd", 0, Seq()) should be (("NON-EXISTENT BEER is missing", defaultKeyboard))
//    service.mkSearchBeerByStyleResponse("NON-EXISTENT STYLE", 0, Seq()) should be (("NON-EXISTENT STYLE is missing", defaultKeyboard))
    
    itemsProvider.beers should have length (3)
    itemsProvider.beers should have length (3)
  }

  private def createItemsProvider(): ItemsProvider = {
    val m = mock[ItemsProvider]
    
    println("as")

    (m.beers _) expects () anyNumberOfTimes() onCall { () =>
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
