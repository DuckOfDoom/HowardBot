import org.duckofdoom.howardbot.bot.Callback.ChangeSorting
import org.duckofdoom.howardbot.bot.{Callback, KeyboardHelper, Sorting}
import org.scalatest.{FlatSpec, Matchers}

class KeyboardSpec extends FlatSpec with Matchers {

  val helper = new KeyboardHelper()

  "KeyboardHelper" should "correctly generate keyboard for empty sorting" in {
    val kb = helper.mkChangeSortingButtons(Seq()).inlineKeyboard

    kb should have length 7

    val allSortings = Sorting.all.toList

    for (i <- 0 to 4) {
      kb(i) should have length 2

      for (i <- allSortings.indices by 2) {
        val cb0 = Callback
          .deserialize[ChangeSorting](kb(i)(0).callbackData.get.getBytes)
          .get
          .asInstanceOf[ChangeSorting]

        val cb1 = Callback
          .deserialize[ChangeSorting](kb(i)(1).callbackData.get.getBytes)
          .get
          .asInstanceOf[ChangeSorting]

        cb0.sorting.map(_.id) should be(Some(allSortings(i).id))
        cb1.sorting.map(_.id) should be(Some(allSortings(i + i + 1).id))
      }
    }
  }
}
