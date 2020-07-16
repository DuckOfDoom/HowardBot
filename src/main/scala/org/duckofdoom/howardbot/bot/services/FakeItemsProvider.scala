package org.duckofdoom.howardbot.bot.services

import java.time.LocalDateTime

import cats.syntax.option._
import org.duckofdoom.howardbot.bot.data.{Beer, BreweryInfo}

class FakeItemsProvider extends ItemsProviderBase {

  override def fillItems(beers: Seq[Beer]): Unit = {

    def mkStyle() = {
      val wrds = faker.Lorem.words(3).map(_.capitalize)
      wrds.head + " / " + wrds(1)
    }

    _beersMap = (0 to 10)
      .map(i => {
        val item = Beer(
          i,
          isInStock = true,
          LocalDateTime.now(),
          LocalDateTime.now(),
          faker.Lorem.words(2).head.capitalize.some,
          i.some,
          (scala.util.Random.nextFloat() * 5f, 5f).some,
          "http://url.com".some,
          "http://pic_url.com".some,
          (scala.util.Random.nextInt(20) / 1000f).some,
          (scala.util.Random.nextInt(100) / 1000f).some,
          BreweryInfo(
            (faker.Company.name + " Brewery").some,
            "http://brewery_url.com".some,
            faker.Address.street_address().some
          ),
          mkStyle().some,
          (200 + scala.util.Random.nextInt(300) + "ml Draft").some,
          ("\u20BD", i * 100f).some,
          faker.Lorem.paragraph().some
        )

        (i, item)
      })
      .toMap

    _beers = _beersMap.values.toList
    _lastRefreshTime = LocalDateTime.now()
  }
}
