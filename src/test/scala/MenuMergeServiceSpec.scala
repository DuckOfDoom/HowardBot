import java.time.LocalDateTime

import org.duckofdoom.howardbot.Config
import org.duckofdoom.howardbot.bot.data.Beer
import org.duckofdoom.howardbot.bot.services.{MenuMergeService, MenuMergeServiceImpl}
import org.scalamock.scalatest.MockFactory
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}

class MenuMergeServiceSpec extends FlatSpec with Matchers with MockFactory {

  implicit val config: Config = stub[Config]
  var now: LocalDateTime = LocalDateTime.now
  val service: MenuMergeService = new MenuMergeServiceImpl(() => now)

  def mkLink(id: Int): Option[String] = {
    val randomString = faker.Internet.domain_name
    s"https://untappd.com/b/$randomString/$id".some
  }


  "MenuMergeService" should "merge in new items correctly" in {

    val newBeers = Seq(
      Beer.ParsedInfo(name = "beer1".some, link = mkLink(1), draftType = "dr".some),
      Beer.ParsedInfo(name = "beer2".some, link = mkLink(2), draftType = "dr".some),
      Beer.ParsedInfo(name = "beer3".some, link = mkLink(3), draftType = "dr".some),
      Beer.ParsedInfo(name = "beer4".some, link = mkLink(4)) // this will get added on deck
    )

    val savedBeers = Seq()
    val (items, log) = service.merge(savedBeers, newBeers)

    items should have length 4
    items.map(_.id) should contain allElementsOf Seq(1, 2, 3, 4)
    every(items.map(_.dateAdded)) should be(now)
    every(items.map(_.dateUpdated)) should be(now)

    log should have length 4

    log should contain(MenuMergeService.newItemInStock.format("beer1"))
    log should contain(MenuMergeService.newItemInStock.format("beer2"))
    log should contain(MenuMergeService.newItemInStock.format("beer3"))
    log should contain(MenuMergeService.newItemOnDeck.format("beer4"))
  }

  it should "merge in new item with old items correctly " in {

    val oldBeerAddTime = LocalDateTime.now.minusDays(5)
    val oldBeerUpdateTime = LocalDateTime.now.minusDays(3)

    val savedBeers = Seq(
      // this one doesn't change
      Beer(1, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer1".some, link = mkLink(1), draftType = "dr".some),
      // this will be is in stock again
      Beer(2, isInStock = false, oldBeerAddTime, oldBeerUpdateTime, "beer2".some, link = mkLink(2), draftType = "dr".some),
      // this one goes out of stock
      Beer(3, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer3".some, link = mkLink(3), draftType = "dr".some),
      // this one was out of stock and should remain so
      Beer(5, isInStock = false, oldBeerAddTime, oldBeerUpdateTime, "beer5".some, link = mkLink(5), draftType = "dr".some),
      // this one was on deck and should move to stock
      Beer(6, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer6".some, link = mkLink(6)),
      // this one was in stock and should move to deck
      Beer(7, isInStock = true, oldBeerAddTime, oldBeerUpdateTime, "beer7".some, link = mkLink(7), draftType = "dr".some),
    )

    val newBeers = Seq(
      Beer.ParsedInfo(name = "beer1".some, link = mkLink(1), draftType = "dr".some),
      Beer.ParsedInfo(name = "beer2".some, link = mkLink(2), draftType = "dr".some),
      // this is a new item
      Beer.ParsedInfo(name = "beer4".some, link = mkLink(4), draftType = "dr".some),
      // this moves from deck to stock
      Beer.ParsedInfo(name = "beer6".some, link = mkLink(6), draftType = "dr".some),
      // this moves from stock to deck
      Beer.ParsedInfo(name = "beer7".some, link = mkLink(7)),
      // this is a new item that goes on deck
      Beer.ParsedInfo(name = "beer8".some, link = mkLink(8)),
    )

    val (items, log) = service.merge(savedBeers, newBeers)

    log should have length 6

    items should have length 8
    items.map(_.id) should contain allElementsOf Seq(1, 2, 3, 5, 6, 7, 8, 9)

    val itemThatDidntChange = items.find(b => b.name.get == "beer1").get
    itemThatDidntChange.id should be(1)
    itemThatDidntChange.isInStock should be(true)
    itemThatDidntChange.dateAdded should be(oldBeerAddTime)
    itemThatDidntChange.dateUpdated should be(oldBeerUpdateTime)

    val itemThatIsInStockAgain = items.find(b => b.name.get == "beer2").get
    itemThatIsInStockAgain.id should be(2)
    itemThatIsInStockAgain.isInStock should be(true)
    itemThatIsInStockAgain.dateAdded should be(oldBeerAddTime)
    itemThatIsInStockAgain.dateUpdated should be(now)
    log should contain(MenuMergeService.inStockAgain.format("beer2"))

    val itemThatIsOutOfStock = items.find(b => b.name.get == "beer3").get
    itemThatIsOutOfStock.id should be(3)
    itemThatIsOutOfStock.isInStock should be(false)
    itemThatIsOutOfStock.dateAdded should be(oldBeerAddTime)
    itemThatIsOutOfStock.dateUpdated should be(now)
    log should contain(MenuMergeService.outOfStock.format("beer3"))

    val itemThatWasOutOfStockAndStillIs = items.find(b => b.name.get == "beer5").get
    itemThatWasOutOfStockAndStillIs.id should be(5)
    itemThatWasOutOfStockAndStillIs.isInStock should be(false)
    itemThatWasOutOfStockAndStillIs.dateAdded should be(oldBeerAddTime)
    itemThatWasOutOfStockAndStillIs.dateUpdated should be(oldBeerUpdateTime)

    val movesFromDeckToStock = items.find(b => b.name.get == "beer6").get
    movesFromDeckToStock.id should be(6)
    movesFromDeckToStock.isInStock should be(true)
    movesFromDeckToStock.isOnDeck should be(false)
    movesFromDeckToStock.dateAdded should be(oldBeerAddTime)
    movesFromDeckToStock.dateUpdated should be(now)
    log should contain(MenuMergeService.fromDeckToStock.format("beer6"))

    val movesFromStockToDeck = items.find(b => b.name.get == "beer7").get
    movesFromStockToDeck.id should be(7)
    movesFromStockToDeck.isInStock should be(true)
    movesFromStockToDeck.isOnDeck should be(true)
    movesFromStockToDeck.dateAdded should be(oldBeerAddTime)
    movesFromStockToDeck.dateUpdated should be(now)
    log should contain(MenuMergeService.fromStockToDeck.format("beer7"))

    val newItem = items.find(b => b.name.get == "beer4").get
    newItem.id should be(8) // Id should be max of all beers
    newItem.isInStock should be(true)
    newItem.dateAdded should be(now)
    newItem.dateUpdated should be(now)
    log should contain(MenuMergeService.newItemInStock.format("beer4"))

    val newItemOnDeck = items.find(b => b.name.get == "beer8").get
    newItemOnDeck.id should be(9) // Id should be max of all beers
    newItemOnDeck.isInStock should be(true)
    newItemOnDeck.dateAdded should be(now)
    newItemOnDeck.dateUpdated should be(now)
    log should contain(MenuMergeService.newItemOnDeck.format("beer8"))
  }
}
