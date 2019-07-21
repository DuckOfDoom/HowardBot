package org.duckofdoom.howardbot.bot.data

trait ItemDataProvider {
  def allItems : Iterable[Item]
  def getItem(itemId:Int) : Option[Item]
}

class PlaceholderItemDataProvider extends ItemDataProvider {
  private val random = scala.util.Random

  val items: Map[Int, Item] = randomItems(10)
  
  override def allItems: Iterable[Item] = items.values
  override def getItem(itemId: Int): Option[Item] = items.get(itemId)
  
  def apply(itemId: Int): Option[Item] = getItem(itemId)
  
  private def randomItems(count:Int): Map[Int, Item] = {
    (0 to count).map(i => {
      val id = random.nextInt()
      val item = PlaceholderItem(
        id,
        faker.Lorem.words(1).head,
        i * 100,
        faker.Company.name,
        faker.Lorem.paragraph()
      )
      
      (id, item)
    }).toMap
  }
}
