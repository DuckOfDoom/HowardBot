package org.duckofdoom.howardbot.bot.services

import org.duckofdoom.howardbot.bot.data.Beer
import org.duckofdoom.howardbot.db.dto.User
import slogging.StrictLogging

trait CartService {

  /**
    *  Add a beer to a cart
    */
  def addToCart(implicit user:User, beer: Beer): Int
  
  /**
    *  Remove a beer from a cart
    */
  def removeFromCart(implicit user:User, beer:Beer) : Int

  /**
    *  Returns cart contents
    */
  def getCartContents(implicit user:User) : Seq[(Beer, Int)]

  /**
    *  Returns total cart price
    */
  def getCartTotalPrice(implicit user:User) : Int
}

//class CartServiceImpl extends CartService
//  with StrictLogging 
//{
//  override def addToCart(implicit user: User, beer: Beer): Int = {
//    user.cart
//    
//  }
//  
//  override def removeFromCart(implicit user: User, beer: Beer): Int = ???
//  override def getCartContents(implicit user: User): Seq[(Beer, Int)] = ???
//  override def getCartTotalPrice(implicit user: User): Int = ???
//}
