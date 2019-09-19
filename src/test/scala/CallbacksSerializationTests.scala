import cats.syntax.option._
import cats.syntax.either._
import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import org.duckofdoom.howardbot.bot.utils.{Callback, Sorting}
import org.duckofdoom.howardbot.bot.data
import org.scalatest.{FunSuite, Matchers}

class CallbacksSerializationTests extends FunSuite with Matchers {

  test("Menu") {
    // Menu
    val menuCallback = Callback.mkMenuCallbackData(10.some, newMessage = false)
    Callback.deserialize(menuCallback.getBytes) match {
      case Some(Callback.Menu(page, newMessage)) =>
        page.get should be(10)
        newMessage should be(false)
      case _ => fail(s"Failed to parse callback '$menuCallback'")
    }
  }

  test("Styles") {
    // Styles
    val stylesCallback = Callback.mkStylesCallbackData(8.some, newMessage = true)
    Callback.deserialize(stylesCallback.getBytes) match {
      case Some(Callback.Menu(_, _)) => fail("This callback should only be parsed as Styles!")
      case Some(Callback.Styles(page, newMessage)) =>
        page.get should be(8)
        newMessage should be(true)
      case _ => fail(s"Failed to parse callback '$stylesCallback'")
    }
  }

  test("ItemsByStyle") {

    // Items by style
    val itemsByStyleCallback = Callback.mkItemsByStyleCallbackData(15, 19)
    Callback.deserialize(itemsByStyleCallback.getBytes) match {
      case Some(Callback.ItemsByStyle(styleId, page)) =>
        styleId should be(15)
        page.toInt should be(19)
      case _ => fail(s"Failed to parse callback '$itemsByStyleCallback'")
    }
  }

  test("Item") {

    // Item
    val itemCallback = Callback.mkItemCallback(
      Beer(
        51,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        BreweryInfo(None, None, None),
        None,
        None,
        None,
        None
      )
    )

    itemCallback should be some

    Callback.deserialize(itemCallback.get.getBytes) match {
      case Some(Callback.SingleBeer(itemType, itemId)) =>
        itemType should be(data.ItemType.Beer)
        itemId should be(51)
      case _ => fail(s"Failed to parse callback '$itemCallback'")
    }
  }

  test("Search beer by name") {

    // Search beers by name
    val beersByNameCallback = Callback.mkSearchBeerByNameCallback("tehbeer", 5)
    Callback.deserialize(beersByNameCallback.getBytes) match {
      case Some(Callback.SearchBeerByName(query, page)) =>
        query should be("tehbeer")
        page should be(5)
      case _ => fail(s"Failed to parse callback '$beersByNameCallback'")
    }
  }

  test("Search beer by style") {
    // Search beers by style
    val beersByStyleCallback = Callback.mkSearchBeerByStyleCallback("tehStyle", 3)
    Callback.deserialize(beersByStyleCallback.getBytes) match {
      case Some(Callback.SearchBeerByStyle(query, page)) =>
        query should be("tehStyle")
        page should be(3)
      case _ => fail(s"Failed to parse callback '$beersByStyleCallback'")
    }
  }
  
  test("Change sorting") {
    
    var changeSortingCallback = Callback.mkChangeSortingCallback(Sorting.byPriceForMlDec.some.asRight)
    Callback.deserialize(changeSortingCallback.getBytes) match {
      case Some(Callback.ChangeSorting(Right(Some(sorting)))) => 
        sorting should be (Sorting.byPriceForMlDec)
      case _ => fail(s"Failed to parse callback '$changeSortingCallback'")
    }
    
    // Case with no sorting
    changeSortingCallback = Callback.mkChangeSortingCallback(().asLeft)
    Callback.deserialize(changeSortingCallback.getBytes) match {
      case Some(Callback.ChangeSorting(Left(_))) => succeed
      case _ => fail(s"Failed to parse callback '$changeSortingCallback'")
    }
  }
}
