package org.duckofdoom.howardbot.bot.data

import org.duckofdoom.howardbot.bot.data.Static.CallbackType.CallbackType

import scala.util.matching.Regex

object Static {
  
  object CallbackType extends Enumeration {
    type CallbackType = Value
    val Menu: Static.CallbackType.Value = Value("Menu")
    val ItemsByStyle: Static.CallbackType.Value = Value("ItemsByStyle")
  }


  private val menuCallbackPrefix = "menu_page_"
  private val itemsByStyleCallbackPrefix = "style_page_"
  
  val menuCallbackRegex: Regex = (menuCallbackPrefix + "(\\d+)").r
  val itemsByStyleCallbackRegex: Regex = (itemsByStyleCallbackPrefix + "(\\w+)_(\\d+)").r
  
  def mkCallbackPrefix(values : String*)(implicit callbackType: CallbackType): String = { 
    callbackType match {
      case CallbackType.Menu => String.format()
      case CallbackType.ItemsByStyle => 
    }
  }

//  def getCallbackPrefix(implicit callbackType: CallbackType) = {
//    callbackType match {
//      case CallbackType.Menu => menuCallbackPrefix
//      case CallbackType.ItemsByStyle => itemsByStyleCallbackPrefix
//    }
//  }
}
