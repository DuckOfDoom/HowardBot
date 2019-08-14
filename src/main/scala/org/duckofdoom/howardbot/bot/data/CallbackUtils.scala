package org.duckofdoom.howardbot.bot.data

import org.duckofdoom.howardbot.bot.data.CallbackUtils.CallbackType.CallbackType

import scala.util.matching.Regex

object CallbackUtils {
  
  object CallbackType extends Enumeration {
    type CallbackType = Value
    val Menu: CallbackUtils.CallbackType.Value = Value("Menu")
    val Styles: CallbackUtils.CallbackType.Value = Value("Styles")
    val ItemsByStyle: CallbackUtils.CallbackType.Value = Value("ItemsByStyle")
  }

  private val menuCallbackPrefix = "menu_page_"
  private val stylesCallbackPrefix = "styles_"
  private val itemsByStyleCallbackPrefix = "style_"
  
  val menuCallbackRegex: Regex = (menuCallbackPrefix + "(\\d+)").r
  val stylesCallbackRegex: Regex = (stylesCallbackPrefix + "(\\d+)").r
  val itemsByStyleCallbackRegex: Regex = (itemsByStyleCallbackPrefix + "(\\w+)_(\\d+)").r
  
  def mkCallbackPrefix(page: Int, values : String*)(implicit callbackType: CallbackType): String = { 
    callbackType match {
      case CallbackType.Menu => menuCallbackPrefix + page
      case CallbackType.Styles => stylesCallbackPrefix + page
      case CallbackType.ItemsByStyle =>
        require(values.nonEmpty, "Can't create ItemsByStyle callback info with empty values!")
        itemsByStyleCallbackPrefix + values(0) + "_" + page
    }
  }
}
