import org.scalatest.FunSuite

import scala.collection.mutable

class CollectionPerformanceTest extends FunSuite {
  test("Performance") {

    var seq     = Seq[String]()
    var list    = List[String]()
    val mList   = mutable.MutableList[String]()
    val lBuffer = mutable.ListBuffer[String]()

    time("Seq") {
      for (_ <- 1 to 10000) {
        seq :+= "Str"
      }
    }

    time("List") {
      for (_ <- 1 to 10000) {
        list :+= "Str"
      }
    }

    time("Mutable List") {
      for (_ <- 1 to 10000) {
        mList += "Str"
      }
    }

    time("List Buffer") {
      for (_ <- 1 to 10000) {
        lBuffer += "Str"
      }
    }
  }

  private def time[R](tag: String)(block: => R): R = {
    val t0     = System.nanoTime()
    val result = block // call-by-name
    val t1     = System.nanoTime()
    println(s"Elapsed time: ${t1 - t0}ns ($tag)")
    result
  }

}
