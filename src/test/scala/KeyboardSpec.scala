import com.bot4s.telegram.models.InlineKeyboardButton
import org.duckofdoom.howardbot.bot.utils.Sorting.Sorting
import org.duckofdoom.howardbot.bot.services.KeyboardHelper
import org.duckofdoom.howardbot.bot.utils.Callback.{ChangeSorting, Menu, Styles}
import org.duckofdoom.howardbot.bot.utils.{Callback, Sorting}
import org.scalatest.{FlatSpec, Matchers}

class KeyboardSpec extends FlatSpec with Matchers {

  val helper = new KeyboardHelper()

  private def getCallback[T <: Callback](i: Int, j: Int)(
      implicit kb: Seq[Seq[InlineKeyboardButton]]
  ) = {
    Callback
      .deserialize[T](kb(i)(j).callbackData.get.getBytes)
      .get
      .asInstanceOf[T]
  }
  
  private def getSortingId(cb: Option[Sorting]) = {
    cb match {
      case Some(sorting) => sorting.id
      case _ => throw new IllegalArgumentException("Derp")
    }
  }

  "KeyboardHelper.mkDefaultButtons" should "have correct layout" in {
    implicit val kb: Seq[Seq[InlineKeyboardButton]] = helper.mkDefaultButtons().inlineKeyboard
    kb should have length 1
    kb(0) should have length 3
    getCallback[Menu](0, 0) should not be null
    getCallback[Styles](0, 1) should not be null
    getCallback[ChangeSorting](0, 2) should not be null
  }

  "KeyboardHelper.mkChangeSortingButtons" should "correctly generate keyboard for empty sorting" in {
    implicit val kb: Seq[Seq[InlineKeyboardButton]] = helper.mkChangeSortingButtons(Seq()).inlineKeyboard

    kb should have length 7

    for (idx <- List(0, 1, 2, 3, 4)) {
      kb(idx) should have length 2
    }

    getCallback[ChangeSorting](0, 0).sorting.map(getSortingId) should be(Right(Sorting.byName.id))
    getCallback[ChangeSorting](0, 1).sorting.map(getSortingId) should be(Right(Sorting.byNameDec.id))

    getCallback[ChangeSorting](1, 0).sorting.map(getSortingId) should be(Right(Sorting.byStyle.id))
    getCallback[ChangeSorting](1, 1).sorting.map(getSortingId) should be(Right(Sorting.byStyleDec.id))

    getCallback[ChangeSorting](2, 0).sorting.map(getSortingId) should be(Right(Sorting.byRating.id))
    getCallback[ChangeSorting](2, 1).sorting.map(getSortingId) should be(Right(Sorting.byRatingDec.id))

    getCallback[ChangeSorting](3, 0).sorting.map(getSortingId) should be(Right(Sorting.byPriceForMl.id))
    getCallback[ChangeSorting](3, 1).sorting.map(getSortingId) should be(Right(Sorting.byPriceForMlDec.id))

    getCallback[ChangeSorting](4, 0).sorting.map(getSortingId) should be(Right(Sorting.byBrewery.id))
    getCallback[ChangeSorting](4, 1).sorting.map(getSortingId) should be(Right(Sorting.byBreweryDec.id))

    kb(5) should have length 1
    getCallback[ChangeSorting](5, 0).sorting should be(Right(None))

    kb(6) should have length 2
    getCallback[Menu](6, 0) should not be null
    getCallback[Styles](6, 1) should not be null
  }

  it should "exclude already present sorting pairs" in {
    implicit val kb: Seq[Seq[InlineKeyboardButton]] =
      helper.mkChangeSortingButtons(Seq(Sorting.byNameDec, Sorting.byPriceForMl)).inlineKeyboard

    kb should have length 5

    for (idx <- List(0, 1, 2)) {
      kb(idx) should have length 2
    }

    getCallback[ChangeSorting](0, 0).sorting.map(getSortingId) should be(Right(Sorting.byStyle.id))
    getCallback[ChangeSorting](0, 1).sorting.map(getSortingId) should be(Right(Sorting.byStyleDec.id))

    getCallback[ChangeSorting](1, 0).sorting.map(getSortingId) should be(Right(Sorting.byRating.id))
    getCallback[ChangeSorting](1, 1).sorting.map(getSortingId) should be(Right(Sorting.byRatingDec.id))

    getCallback[ChangeSorting](2, 0).sorting.map(getSortingId) should be(Right(Sorting.byBrewery.id))
    getCallback[ChangeSorting](2, 1).sorting.map(getSortingId) should be(Right(Sorting.byBreweryDec.id))

    kb(3) should have length 1
    getCallback[ChangeSorting](3, 0).sorting should be(Right(None))

    kb(4) should have length 2
    getCallback[Menu](4, 0) should not be null
    getCallback[Styles](4, 1) should not be null
  }
}
