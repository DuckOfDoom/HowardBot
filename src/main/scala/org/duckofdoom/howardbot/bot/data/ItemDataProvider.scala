package org.duckofdoom.howardbot.bot.data

trait ItemDataProvider {
  def allItems: Iterable[Item]
  def getItem(itemId: Int): Option[Item]
}

class PlaceholderItemDataProvider extends ItemDataProvider {
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
        val item = PlaceholderItem(
          i,
          faker.Lorem.words(2).head.capitalize,
          mkStyle(),
          faker.Company.name + " Brewery",
          i * 100,
          faker.Lorem.paragraph()
        )

        (i, item)
      })
      .toMap
  }
}
