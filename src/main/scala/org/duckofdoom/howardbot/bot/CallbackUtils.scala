package org.duckofdoom.howardbot.bot

import org.duckofdoom.howardbot.bot.CallbackUtils.CallbackType.CallbackType

import scala.util.matching.Regex

object CallbackUtils {

  object CallbackType extends Enumeration {
    type CallbackType = Value
    val Menu: CallbackUtils.CallbackType.Value         = Value("Menu")
    val Styles: CallbackUtils.CallbackType.Value       = Value("Styles")
    val ItemsByStyle: CallbackUtils.CallbackType.Value = Value("ItemsByStyle")
  }

  private val menuCallbackPrefix         = "menu_page_"
  private val stylesCallbackPrefix       = "styles_page_"
  private val itemsByStyleCallbackPrefix = "items_by_style_page_"

  val menuCallbackRegex: Regex   = (menuCallbackPrefix + "(\\d+)").r
  val stylesCallbackRegex: Regex = (stylesCallbackPrefix + "(\\d+)").r
  
  // Used to check if we can successfully use and parse this style from callback query
  val styleValidationRegex: Regex = "([A-Za-z0-9\\-\\\\/\\(\\)]+)".r
  val itemsByStyleCallbackRegex: Regex = (itemsByStyleCallbackPrefix + styleValidationRegex.regex + "_(\\d+)").r

  def mkCallbackPrefix(page: Int, payload: String)(implicit callbackType: CallbackType): String = {
    callbackType match {
      case CallbackType.Menu   => menuCallbackPrefix + page
      case CallbackType.Styles => stylesCallbackPrefix + page
      case CallbackType.ItemsByStyle =>
        require(payload.nonEmpty, "Can't create ItemsByStyle callback info with empty values!")
        itemsByStyleCallbackPrefix + payload + "_" + page
    }
  }
}
