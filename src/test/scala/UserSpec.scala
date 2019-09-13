import org.duckofdoom.howardbot.bot.Sorting
import org.duckofdoom.howardbot.db.dto.{User, UserState}
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._
import io.circe.parser.decode
import io.circe.{Decoder, Encoder}

class UserSpec extends FlatSpec with Matchers {
  
  def createDefaultUser(userState: UserState): User ={
    User(1,2, "John", "Doe", "@Gsom", userState)
  }

  "UserState" should "have modifiable menu page" in {
    val user = createDefaultUser(UserState())
    val modified = user.withMenuPage(1337)
    
    modified should not be user
    modified.state.menuPage should be (1337)
  }
  
  it should "have modifiable styles page" in {

    val user = createDefaultUser(UserState())
    val modified = user.withStylesPage(7331)

    modified should not be user
    modified.state.stylesPage should be (7331)
  }
  
  it should "have modifiable sorting" in {

    val user = createDefaultUser(UserState())
    val modified = user.withAddedSorting(Sorting.byPriceForMlDec)

    modified should not be user
    modified.state.sorting should contain theSameElementsAs Seq(Sorting.byPriceForMlDec)
    
    val modified2 = modified.withEmptySorting()
    modified2.state.sorting should be (empty)
  }


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
