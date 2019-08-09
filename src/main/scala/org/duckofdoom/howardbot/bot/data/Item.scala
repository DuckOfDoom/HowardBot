package org.duckofdoom.howardbot.bot.data

trait Item {
  val id: Int
  val menuOrder: Option[Int]
  val name: Option[String]
  val rating: Option[(Float, Float)]
  val link: Option[String]
  val pictureLink: Option[String]
  val abv: Option[Float]
  val ibu: Option[Float]
  val breweryInfo: BreweryInfo
  val style: Option[String]
  val draftType: Option[String]
  val price: Option[(String, Float)]
  val description: Option[String]

  override def toString: String = {

    def toString[T](name: String, v: Option[T]): String = {
      s"$name = ${if (v.isDefined) v.get.toString else "?"}\n"
    }

    val sb = new StringBuilder()
    sb.append(s"Id = $id\n")
    sb.append(toString("menuOrder", menuOrder))
    sb.append(toString("name", name))
    sb.append(toString("rating", rating))
    sb.append(toString("style", style))
    sb.append(toString("link", link))
    sb.append(toString("pictureLink", pictureLink))
    sb.append(toString("abv", abv))
    sb.append(toString("ibu", ibu))
    sb.append(toString("draftType", draftType))
    sb.append("BreweryInfo:\n")
    sb.append(s"\t ${toString("name", breweryInfo.name)}")
    sb.append(s"\t ${toString("link", breweryInfo.link)}")
    sb.append(s"\t ${toString("location", breweryInfo.location)}")

    sb.append(toString("price", price))
    sb.append(toString("description", description))

    sb.toString
  }
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
    rating: Option[(Float, Float)],
    link: Option[String],
    pictureLink: Option[String],
    abv: Option[Float],
    ibu: Option[Float],
    breweryInfo: BreweryInfo,
    style: Option[String],
    draftType: Option[String],
    price: Option[(String, Float)],
    description: Option[String],
) extends Item {
}
