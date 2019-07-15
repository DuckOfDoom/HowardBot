package org.duckofdoom.howardbot

class BotConfig(A:String, B:String) {
  var a: String = A
  var b: String = B
  var inner : InnerConfig = InnerConfig(A.length, B.length)
}

case class InnerConfig(x:Int, y:Int) {
}
