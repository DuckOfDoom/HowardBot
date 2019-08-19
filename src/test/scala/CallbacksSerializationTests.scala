import org.duckofdoom.howardbot.bot.Callback.{ItemsByStyle, Menu, Styles}
import org.scalatest.{FunSuite, Matchers}
import cats.syntax.option._
import org.duckofdoom.howardbot.bot.Callback
import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType
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

    val menuCallback = PaginationUtils.mkCallbackData(10.some, None)(CallbackType.Menu)
    Callback.deserialize(menuCallback.getBytes) match {
      case Some(Callback.Menu(page, newMessage)) =>
        page.get should be(10)
        newMessage should be(false)
      case _ => fail(s"Failed to parse callback '$menuCallback'")
    }

    val stylesCallback = PaginationUtils.mkCallbackData(8.some, None)(CallbackType.Styles)
    Callback.deserialize(stylesCallback.getBytes) match {
      case Some(Callback.Menu(_, _)) => fail("This callback should only be parsed as Styles!")
      case Some(Callback.Styles(page, newMessage)) =>
        page.get should be(8)
        newMessage should be(false)
      case _ => fail(s"Failed to parse callback '$stylesCallback'")
    }

    val itemsByStyleCallback =
      PaginationUtils.mkCallbackData(15.some, 19.some)(CallbackType.ItemsByStyle)
    Callback.deserialize(itemsByStyleCallback.getBytes) match {
      case Some(Callback.ItemsByStyle(styleId, page)) =>
        page.toInt should be(15)
        styleId should be(19)
      case _ => fail(s"Failed to parse callback '$itemsByStyleCallback'")
    }

  }
}
