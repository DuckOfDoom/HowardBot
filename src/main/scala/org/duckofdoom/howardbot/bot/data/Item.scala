package org.duckofdoom.howardbot.bot.data

import org.duckofdoom.howardbot.bot.data.ItemType.ItemType

object ItemType extends Enumeration {
  type ItemType = Value
  val Beer, Style = Value
}

trait Item {
  val id: Int
  val itemType : ItemType
}

object Style {
  implicit def ordering[A <: Style]: Ordering[A] = Ordering.by(_.name)
}

case class Style(id: Int, name: String) extends Item {
  override val itemType: ItemType = ItemType.Style
}

case class BreweryInfo(
    name: Option[String],
    link: Option[String],
    location: Option[String]
)

case class Beer(
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
  
  override val itemType: ItemType = ItemType.Beer

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
