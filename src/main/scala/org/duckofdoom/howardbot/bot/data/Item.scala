package org.duckofdoom.howardbot.bot.data

import java.time.LocalDateTime

import org.duckofdoom.howardbot.bot.data.ItemType.ItemType
import org.duckofdoom.howardbot.utils.TimeUtils
import io.circe._
import io.circe.generic.semiauto._

object ItemType extends Enumeration {
  type ItemType = Value
  val Beer, Style = Value
}

trait Item {
  val id: Int
  val itemType: ItemType
}

object Style {
  implicit def ordering[A <: Style]: Ordering[A] = Ordering.by(_.name)
}

case class Style(id: Int, name: String) extends Item {
  override val itemType: ItemType = ItemType.Style
}

case class BreweryInfo(
    name: Option[String] = None,
    link: Option[String] = None,
    location: Option[String] = None
)

object Beer {
  implicit val bEncoder: Encoder[BreweryInfo] = deriveEncoder[BreweryInfo]
  implicit val encoder: Encoder[Beer]         = deriveEncoder[Beer]

  implicit val bDecoder: Decoder[BreweryInfo] = deriveDecoder[BreweryInfo]
  implicit val decoder: Decoder[Beer]         = deriveDecoder[Beer]

  case class ParsedInfo(
      dateAdded: LocalDateTime = LocalDateTime.MIN,
      name: Option[String] = None,
      menuOrder: Option[Int] = None,
      rating: Option[(Float, Float)] = None,
      link: Option[String] = None,
      pictureLink: Option[String] = None,
      abv: Option[Float] = None,
      ibu: Option[Float] = None,
      breweryInfo: BreweryInfo = BreweryInfo(),
      style: Option[String] = None,
      draftType: Option[String] = None,
      price: Option[(String, Float)] = None,
      description: Option[String] = None
  ) {
    def isOnDeck: Boolean = draftType.isEmpty
  }

  def fromParsedInfo(
      id: Int,
      isInStock: Boolean,
      dateAdded: LocalDateTime,
      dateUpdated: LocalDateTime,
      info: ParsedInfo
  ): Beer = {
    Beer(
      id,
      isInStock,
      dateAdded,
      dateUpdated,
      info.name,
      info.menuOrder,
      info.rating,
      info.link,
      info.pictureLink,
      info.abv,
      info.ibu,
      info.breweryInfo,
      info.style,
      info.draftType,
      info.price,
      info.description
    )
  }

  def fromAnotherBeerWithUpdatedInfo(
      isInStock: Boolean,
      dateUpdated: LocalDateTime,
      beer: Beer,
      parsedInfo: ParsedInfo
  ): Beer = {
    Beer(
      beer.id,
      isInStock,
      beer.dateAdded,
      dateUpdated,
      parsedInfo.name,
      parsedInfo.menuOrder,
      parsedInfo.rating,
      parsedInfo.link,
      parsedInfo.pictureLink,
      parsedInfo.abv,
      parsedInfo.ibu,
      parsedInfo.breweryInfo,
      parsedInfo.style,
      parsedInfo.draftType,
      parsedInfo.price,
      parsedInfo.description
    )
  }

  def fromAnotherBeer(isInStock: Boolean, beer: Beer): Beer = {
    fromAnotherBeerWithUpdatedTime(isInStock, beer.dateUpdated, beer)
  }

  def fromAnotherBeerWithUpdatedTime(isInStock: Boolean, dateUpdated: LocalDateTime, beer: Beer): Beer = {
    Beer(
      beer.id,
      isInStock,
      beer.dateAdded,
      dateUpdated,
      beer.name,
      beer.menuOrder,
      beer.rating,
      beer.link,
      beer.pictureLink,
      beer.abv,
      beer.ibu,
      beer.breweryInfo,
      beer.style,
      beer.draftType,
      beer.price,
      beer.description
    )
  }
}

case class Beer (
    id: Int,
    isInStock: Boolean,
    dateAdded: LocalDateTime = LocalDateTime.MIN,
    dateUpdated: LocalDateTime = LocalDateTime.MIN,
    name: Option[String] = None,
    menuOrder: Option[Int] = None, 
    rating: Option[(Float, Float)] = None,
    link: Option[String] = None,
    pictureLink: Option[String] = None,
    abv: Option[Float] = None,
    ibu: Option[Float] = None,
    breweryInfo: BreweryInfo = BreweryInfo(),
    style: Option[String] = None,
    draftType: Option[String] = None,
    price: Option[(String, Float)] = None,
    description: Option[String] = None
) extends Item {

  def isOnDeck: Boolean = draftType.isEmpty // TODO: Probably add a check for price here too. How can we sell without a price?

  override val itemType: ItemType = ItemType.Beer

  override def toString: String = {

    def toString[T](name: String, v: Option[T]): String = {
      s"$name = ${if (v.isDefined) v.get.toString else "?"}\n"
    }

    val sb = new StringBuilder()
    sb.append(s"id = $id\n")
    sb.append(s"isInStock = $isInStock\n")
    sb.append(s"dateAdded = ${TimeUtils.formatDateTime(dateAdded)}\n")
    sb.append(s"dateUpdated = ${TimeUtils.formatDateTime(dateUpdated)}\n")
    sb.append(toString("name", name))
    sb.append(toString("menuOrder", menuOrder))
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
