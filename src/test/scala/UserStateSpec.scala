import io.circe.syntax._
import io.circe.parser.decode
import io.circe.{Decoder, Encoder}
import org.duckofdoom.howardbot.bot.Sorting
import org.duckofdoom.howardbot.db.dto.UserState
import org.scalatest.{FlatSpec, Matchers}

class UserStateSpec extends FlatSpec with Matchers {
 
  implicit val decoder: Decoder[UserState] = UserState.decoder
  implicit val encoder: Encoder[UserState] = UserState.encoder

  "UserState" should "be serialized and deserialized correctly" in {

    val state        = UserState(5, 7, List(Sorting.byBrewery, Sorting.byPriceForMlDec))
    val serialized   = state.asJson
    
    serialized.as[UserState] should be(Right(state))
  }

  it should "be deserialized correctly with missing fields" in {

    val stateJson =
      """ {
      "menuPage" : 5,
      "stylesPage" : 7
    }
  """
    
    decode[UserState](stateJson) should be (Right(UserState(5, 7, List())))
  }

}
