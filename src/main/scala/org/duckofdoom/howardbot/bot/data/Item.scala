package org.duckofdoom.howardbot.bot.data

trait Item {
  val id: Int
  val name: String
  val link: String
  val pictureLink: String
  val abv: Float
  val ibu: Float
  val breweryInfo : BreweryInfo
  val style: String
  val draftType: String
  val price: (String, Float)
  val description: String
}

case class BreweryInfo(
    name: String,
    link: String,
    location: String
)

case class MenuItem(
   id: Int,
   name: String,
   link: String,
   pictureLink: String,
   abv: Float,
   ibu: Float,
   breweryInfo: BreweryInfo,
   style: String,
   draftType: String,
   price: (String, Float),
   description: String,
) extends Item
