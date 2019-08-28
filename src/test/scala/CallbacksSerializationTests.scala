import org.duckofdoom.howardbot.bot.Callback.{ItemsByStyle, Menu, Styles}
import org.scalatest.{FunSuite, Matchers}
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.{Callback, CallbackUtils, data}
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
import org.duckofdoom.howardbot.bot.data.ItemType
import org.duckofdoom.howardbot.utils.PaginationUtils

class CallbacksSerializationTests extends FunSuite with Matchers {

  test("Menu serialized correctly") {
    val menu1   = Menu(5.some, newMessage = true)
    val result1 = Callback.deserialize(menu1.serialize()).map(_.asInstanceOf[Menu])

    result1 should be (Menu(5.some, newMessage = true).some) 

    val menu2   = Menu(None, newMessage = false)
    val result2 = Callback.deserialize(menu2.serialize()).map(_.asInstanceOf[Menu])

    result2 should be (Menu(None, newMessage = false).some)
  }

  test("Styles serialized correctly") {
    val menu1   = Styles(5.some, newMessage = true)
    val result1 = Callback.deserialize(menu1.serialize()).map(_.asInstanceOf[Styles])

    result1 should be (Styles(5.some, newMessage = true).some)

    val menu2   = Styles(None, newMessage = false)
    val result2 = Callback.deserialize(menu2.serialize()).map(_.asInstanceOf[Styles])
    result2 should be (Styles(None, newMessage = false).some)
  }

  test("ItemsByStyle serialized correctly") {
    val menu1   = ItemsByStyle(5, 7)
    val result1 = Callback.deserialize(menu1.serialize()).map(_.asInstanceOf[ItemsByStyle])

    result1 should be (ItemsByStyle(5, 7).some)
  }
  
  test("Callbacks are parsed back and forth.") {

    val menuCallback = CallbackUtils.mkMenuCallbackData(10.some, newMessage = false)
    Callback.deserialize(menuCallback.getBytes) match {
      case Some(Callback.Menu(page, newMessage)) =>
        page.get should be(10)
        newMessage should be(false)
      case _ => fail(s"Failed to parse callback '$menuCallback'")
    }

    val stylesCallback = CallbackUtils.mkStylesCallbackData(8.some, newMessage = true)
    Callback.deserialize(stylesCallback.getBytes) match {
      case Some(Callback.Menu(_, _)) => fail("This callback should only be parsed as Styles!")
      case Some(Callback.Styles(page, newMessage)) =>
        page.get should be(8)
        newMessage should be(true)
      case _ => fail(s"Failed to parse callback '$stylesCallback'")
    }

    val itemsByStyleCallback = CallbackUtils.mkItemsByStyleCallbackData(15, 19)
    Callback.deserialize(itemsByStyleCallback.getBytes) match {
      case Some(Callback.ItemsByStyle(styleId, page)) =>
        styleId should be(15)
        page.toInt should be(19)
      case _ => fail(s"Failed to parse callback '$itemsByStyleCallback'")
    }
    
    val itemCallback = CallbackUtils.mkItemCallback(ItemType.Beer, 51)
    Callback.deserialize(itemCallback.getBytes) match {
      case Some(Callback.Item(itemType, itemId)) =>
        itemType should be (data.ItemType.Beer)
        itemId should be (51)
      case _ => fail(s"Failed to parse callback '$itemsByStyleCallback'")
    }

  }
}
