import java.time.LocalDateTime

import cats.syntax.either._
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data
import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}
import org.duckofdoom.howardbot.bot.utils.{Callback, Sorting}
import org.scalatest.{FlatSpec, Matchers}

class CallbacksSpec extends FlatSpec with Matchers {
  
  "'Menu' callback" should "be serialized and deserialized correctly" in {
    // Menu
    var menuCallback = Callback.mkMenuCallbackData(10.some, newMessage = true)
    Callback.deserialize(menuCallback.getBytes) match {
      case Some(Callback.Menu(page, newMessage)) =>
        page should be(Some(10))
        newMessage should be(true)
      case _ => fail(s"Failed to parse callback '$menuCallback'")
    }
    
    // NONE was not serialized correctly!
    menuCallback = Callback.mkMenuCallbackData(None, newMessage = false)
    Callback.deserialize(menuCallback.getBytes) match {
      case Some(Callback.Menu(page, newMessage)) =>
        page should be (None)
        newMessage should be(false)
      case _ => fail(s"Failed to parse callback '$menuCallback'")
    }
  }

  "'Styles' callback" should "be serialized and deserialized correctly" in {
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

  "'ItemsByStyle' callback" should "be serialized and deserialized correctly" in {

    // Items by style
    val itemsByStyleCallback = Callback.mkItemsByStyleCallbackData(15, 19)
    Callback.deserialize(itemsByStyleCallback.getBytes) match {
      case Some(Callback.ItemsByStyle(styleId, page)) =>
        styleId should be(15)
        page.toInt should be(19)
      case _ => fail(s"Failed to parse callback '$itemsByStyleCallback'")
    }
  }

  "'Single Beer' callback" should "be serialized and deserialized correctly" in {

    // Item
    val itemCallback = Callback.mkSingleItemCallback(
      Beer(
        51,
        isInStock = true,
        LocalDateTime.MIN,
        LocalDateTime.MIN,
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
      case Some(Callback.SingleItem(itemType, itemId)) =>
        itemType should be(data.ItemType.Beer)
        itemId should be(51)
      case _ => fail(s"Failed to parse callback '$itemCallback'")
    }
  }

  "'Search beer by name' callback" should "be serialized and deserialized correctly" in {

    // Search beers by name
    val beersByNameCallback = Callback.mkSearchBeerByNameCallback("tehbeer", 5)
    Callback.deserialize(beersByNameCallback.getBytes) match {
      case Some(Callback.SearchBeerByName(query, page)) =>
        query should be("tehbeer")
        page should be(5)
      case _ => fail(s"Failed to parse callback '$beersByNameCallback'")
    }
  }

  "'Search beer by style' callback" should "be serialized and deserialized correctly" in {
    // Search beers by style
    val beersByStyleCallback = Callback.mkSearchBeerByStyleCallback("tehStyle", 3)
    Callback.deserialize(beersByStyleCallback.getBytes) match {
      case Some(Callback.SearchBeerByStyle(query, page)) =>
        query should be("tehStyle")
        page should be(3)
      case _ => fail(s"Failed to parse callback '$beersByStyleCallback'")
    }
  }
  
  "'Settings' callback" should "be serialized and deserialized correctly" in {
    // Search beers by style
    val settingsCallback = Callback.mkSettingsCallback()
    Callback.deserialize(settingsCallback.getBytes) match {
      case Some(Callback.Settings()) => succeed
      case _ => fail(s"Failed to parse callback '$settingsCallback'")
    }
  }
  
  "'Change sorting' callback" should "be serialized and deserialized correctly" in {
    
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
  
  "'ToggleNotifications' callback" should "be serialized and deserialized correctly" in {
    // Search beers by style
    val toggleNotificationsCallback = Callback.mkToggleNotificationsCallback()
    Callback.deserialize(toggleNotificationsCallback.getBytes) match {
      case Some(Callback.ToggleNotifications()) => succeed
      case _ => fail(s"Failed to parse callback '$toggleNotificationsCallback'")
    }
  }
}
