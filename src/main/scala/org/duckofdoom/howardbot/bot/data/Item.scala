package org.duckofdoom.howardbot.bot.data

trait Item {
  val id: Int
  val menuOrder: Option[Int]
  val name: Option[String]
  val link: Option[String]
  val pictureLink: Option[String]
  val abv: Option[Float]
  val ibu: Option[Float]
  val breweryInfo: BreweryInfo
  val style: Option[String]
  val draftType: Option[String]
  val price: Option[(String, Float)]
  val description: Option[String]
}

case class BreweryInfo(
    name: Option[String],
    link: Option[String],
    location: Option[String]
)

case class MenuItem(
    id: Int,
    menuOrder: Option[Int],
    name: Option[String],
    link: Option[String],
    pictureLink: Option[String],
    abv: Option[Float],
    ibu: Option[Float],
    breweryInfo: BreweryInfo,
    style: Option[String],
    draftType: Option[String],
    price: Option[(String, Float)],
    description: Option[String],
) extends Item
