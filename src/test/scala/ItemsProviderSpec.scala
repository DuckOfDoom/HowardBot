import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo, Item, ItemsProviderImpl, Style}
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.option._

class ItemsProviderSpec extends FlatSpec with Matchers {
  
  val provider = new ItemsProviderImpl()
  var items: Seq[Beer] = generateItems()
  provider.fillItems(items)
  
  var beers: Seq[Beer] = items.filter(b => b.breweryInfo.name.isDefined).toList
  
  "ItemsProvider" should "ignore items without breweries (food)" in { 
    (items.length - provider.beers.length) should be (1)
  }
  
  it should "have correct refresh time" in {
    provider.lastRefreshTime.getSecond should be (LocalDateTime.now.getSecond +- 1)
  }
  
  it should "contain all beers" in {
    provider.beers.length should be (beers.length)
  }
  
  it should "provide available beers" in {
    provider.availableBeers.length should be (4)
  }
  
  it should "return correct number of styles" in {
    provider.styles.length should be (9)
  }
  
  it should "return correct number of available styles" in {
    provider.getAvailableStyles(false).length should be (3)
  }

  it should "return correct number of available short styles" in {
    provider.getAvailableStyles(true).length should be (2)
  }
  
  it should "have correct ids for styles" in {
    val styleName = "Derp - Lerp"
    val styleId = provider.getStyleId(styleName)
    
    styleId shouldBe defined
    
    provider.getStyle(styleId.get) should be (Some(Style(styleId.get, styleName)))
  }
  
  it should "return beers by id" in {
    for (i <- 1 to 6) {
      provider.getBeer(i).flatMap(_.name) should be (s"beer$i".some)
    }
    
    provider.getBeer(999) shouldBe None
  }

  it should "find beers by style id" in {

    val beers = provider.findBeerByStyleId(5, includeOutOfStock = true)
    beers.map(_.name.get) should contain allElementsOf Seq("beer4", "beer5")
    
    val beersInStockOnly = provider.findBeerByStyleId(5)
    beersInStockOnly.map(_.name.get) should contain allElementsOf Seq("beer4")
    
    // Everything "Derp"
    val shortStyleBeers = provider.findBeerByStyleId(4, includeOutOfStock = true)
    shortStyleBeers.map(_.name.get) should contain allElementsOf Seq("beer4", "beer5", "beer6")

    val shortStyleBeersInStockOnly = provider.findBeerByStyleId(4)
    shortStyleBeersInStockOnly.map(_.name.get) should contain allElementsOf Seq("beer4")
  }

  private def generateItems() : Seq[Beer] = {
    val br = BreweryInfo("herp".some, "derp".some)
    
    Seq(
      new Beer(id = 1, isInStock = true, name = "beer1".some, style = "Herp - Dorp - Durp".some, breweryInfo = br, draftType = "100ml".some),
      new Beer(id = 2, isInStock = true, name = "beer2".some, style = "Herp - Dorp - Schmorp".some, breweryInfo = br, draftType = "100ml".some),
      new Beer(id = 3, isInStock = true, name = "beer3".some, style = "Herp - Dorp - Schmorp".some, breweryInfo = br, draftType = "100ml".some),

      new Beer(id = 4, isInStock = true, name = "beer4".some, style = "Derp - Lerp".some, breweryInfo = br, draftType = "100ml".some),
      new Beer(id = 5, isInStock = false, name = "beer5".some, style = "Derp - Lerp" .some, breweryInfo = br, draftType = "100ml".some),
      new Beer(id = 6, isInStock = false, name = "beer6".some, style = "Derp - Dorp".some, breweryInfo = br, draftType = "100ml".some),
      
      new Beer(id = 7, isInStock = false, name = "beer7".some, style = "Slerp - Dorp".some, breweryInfo = br, draftType = "100ml".some),
      
      new Beer(id = 8, isInStock = false, name = "beer8".some, style = "Worp-dorp".some, breweryInfo = br, draftType = "100ml".some),
      
      // Not a beer since no brewery
      new Beer(id = 999, isInStock = true, name = "beer999".some, style = "Herp - Dorp - Schmorp".some, draftType = "100ml".some)
    )
  }

}
