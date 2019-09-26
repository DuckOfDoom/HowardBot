import java.time.LocalDateTime

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import org.duckofdoom.howardbot.bot.services.{MenuMergeService, MenuMergeServiceImpl}
import org.scalamock.scalatest.MockFactory
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}

class MenuMergeServiceSpec extends FlatSpec with Matchers with MockFactory {

  implicit val config: Config   = stub[Config]
  var now: LocalDateTime        = LocalDateTime.now
  val service: MenuMergeService = new MenuMergeServiceImpl(() => now)

  "MenuMergeService" should "merge in new items correctly" in {

    val newBeers = Seq(
      Beer.ParsedInfo(name = "beer1".some),
      Beer.ParsedInfo(name = "beer2".some),
      Beer.ParsedInfo(name = "beer3".some)
    )

    val savedBeers   = Seq()
    val (items, log) = service.merge(savedBeers, newBeers)

    items should have length 3
    items.map(_.id) should contain allElementsOf Seq(1, 2, 3)
    every(items.map(_.dateAdded)) should be(now)
    every(items.map(_.dateUpdated)) should be(now)

    log should have length 3

    for (n <- items.map(_.name.get)) {
      log should contain(MenuMergeService.newItem.format(n))
    }
  }

  it should "merge in new item with old items correctly " in {

    val oldBeerAddTime    = LocalDateTime.now.minusDays(5)
    val oldBeerUpdateTime = LocalDateTime.now.minusDays(3)

    val newBeers = Seq(
      Beer.ParsedInfo(name = "beer1".some),
      Beer.ParsedInfo(name = "beer2".some),
      Beer.ParsedInfo(name = "beer4".some)
    )

    val savedBeers = Seq(
      // this one doesnt change
      Beer(1, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer1".some),
      // this will be is in stock again
      Beer(2, isInStock = false, oldBeerAddTime, oldBeerUpdateTime, "beer2".some),
      // this one goes out of stock
      Beer(5, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer3".some)
    )

    val (items, log) = service.merge(savedBeers, newBeers)
    
    items should have length 4
    items.map(_.id) should contain allElementsOf Seq(1, 2, 5, 6)
    
    val itemThatDidintChange = items.find(b => b.name.get == "beer1").get
    itemThatDidintChange.isInStock should be (true)
    itemThatDidintChange.dateAdded should be (oldBeerAddTime)
    itemThatDidintChange.dateUpdated should be (oldBeerUpdateTime)
    
    val itemThatIsInStockAgain = items.find(b => b.name.get == "beer2").get
    itemThatDidintChange.isInStock should be (true)
    itemThatIsInStockAgain.dateAdded should be (oldBeerAddTime)
    itemThatIsInStockAgain.dateUpdated should be (now)
    
    val itemThatIsOutOfStock = items.find(b => b.name.get == "beer3").get
    itemThatIsOutOfStock.isInStock should be (false)
    itemThatIsOutOfStock.dateAdded should be (oldBeerAddTime)
    itemThatIsOutOfStock.dateUpdated should be (now)

    val brandNewItem = items.find(b => b.name.get == "beer4").get
    brandNewItem.id should be (6)
    brandNewItem.isInStock should be (true)
    brandNewItem.dateAdded should be (now)
    brandNewItem.dateUpdated should be (now)

    log should have length 3

//    for (n <- items.map(_.name.get)) {
//      log should contain(MenuMergeService.newItem.format(n))
//    }
  }
}
