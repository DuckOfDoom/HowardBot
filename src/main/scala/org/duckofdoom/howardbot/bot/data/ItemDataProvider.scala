package org.duckofdoom.howardbot.bot.data

trait ItemDataProvider {
  def allItems: Iterable[Item]
  def getItem(itemId: Int): Option[Item]
}

class FakeItemDataProvider extends ItemDataProvider {
  val items: Map[Int, Item] = randomItems(10)

  override def allItems: Iterable[Item]           = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)

  def apply(itemId: Int): Option[Item] = getItem(itemId)

  private def randomItems(count: Int): Map[Int, Item] = {
    
    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds(0) + " / " + wrds(1)
    }
    
    (0 to count)
      .map(i => {
        val item = MenuItem(
          i,
          faker.Lorem.words(2).head.capitalize,
          "http://url.com",
          "http://pic_url.com",
          scala.util.Random.nextInt(20),
          scala.util.Random.nextInt(100) / 1000 ,
          BreweryInfo(
            faker.Company.name + " Brewery",
            "http://brewery_url.com",
            faker.Address.street_address()
          ),
          mkStyle(),
          200 + scala.util.Random.nextInt(300) + "ml Draft",
          ("\u20BD", i * 100),
          faker.Lorem.paragraph()
        )

        (i, item)
      })
      .toMap
  }
}
