// I've left the println's in so you can see what it is doing. They can be removed or swapped for a logger

object ShopCart {

  sealed trait Offer {
    def calcUnitsWithOffer(quantity: Int, item: Item): Int
  }

  class NoOffer extends Offer {
    override def calcUnitsWithOffer(quantity: Int, item: Item): Int = quantity
  }

  class BuyOneGetOneFree extends Offer {
    override def calcUnitsWithOffer(quantity: Int, item: Item): Int = {
      val redeemedOfferUnits = quantity / 2
      val remainderUnits = quantity - (redeemedOfferUnits * 2)
      val totalUnitsToBill = redeemedOfferUnits + remainderUnits
      if (redeemedOfferUnits > 0) println(s"BuyOneGetOneFree offer applied for '${item.sku}' quantity=$quantity  redeemedOfferUnits=$redeemedOfferUnits  remainderUnits=$remainderUnits  totalUnitsToBill=$totalUnitsToBill")
      totalUnitsToBill
    }
  }

  class BuyTwoGetOneFree extends Offer {
    override def calcUnitsWithOffer(quantity: Int, item: Item): Int = {
      val redeemedOfferCount = quantity / 3
      val remainderUnits = quantity - (redeemedOfferCount * 3)
      val totalUnitsToBill = (redeemedOfferCount * 2) + remainderUnits
      if (redeemedOfferCount > 0) println(s"BuyTwoGetOneFree offer applied for '${item.sku}'  quantity=$quantity  redeemedOfferCount=$redeemedOfferCount  remainderUnits=$remainderUnits  totalUnitsToBill=$totalUnitsToBill")
      totalUnitsToBill
    }
  }

  sealed trait Offer2 {
    def calcUnitsWithOffer2(m: Map[String, Int], products: Map[String, Item], key: String): Int
  }

  class NoOffer2 extends Offer2 {
    override def calcUnitsWithOffer2(m: Map[String, Int], products: Map[String, Item], key: String): Int = m(key)
  }

  class OfferCheapestFree(p1: String, p2: String) extends Offer2 {
    override def calcUnitsWithOffer2(m: Map[String, Int], products: Map[String, Item], key: String): Int = {
      if (m.isDefinedAt(p1) && m.isDefinedAt(p2) && products(p1).price != products(p2).price) {
        val cheapest = if (products(p1).price < products(p2).price) p1 else p2
        val freeUnits = m(p1).min(m(p2))

        if (key.equals(cheapest)) {
          println(s"Offer CheapestFree applied for $cheapest quantity=$m(key) ,  freeUnits=$freeUnits ,  new quantity = ${m(key) - freeUnits}  p1=$p1 p1Price=${products(p1).price} | p2=$p2 p2Price=${products(p2).price} | cheapest=$cheapest")
          m(key) - freeUnits
        } else m(key)

      } else m(key)
    }
  }

  class Item(val sku: String, val price: BigDecimal, val offer: Offer, val offer2: Offer2)

  def calcTotal(items: List[String], products: Map[String, Item]): BigDecimal = {
    val itemCounts = items.map(_.trim.toLowerCase).filter(products.isDefinedAt).groupMapReduce(identity)(_ => 1)(_ + _)
    println(s"itemCounts= $itemCounts\n-------")

    val itemCountsAfterOffers1 = itemCounts.map((k, v) => (k, products(k).offer.calcUnitsWithOffer(v, products(k))))
    println(s"itemCountsAfterOffers1= $itemCountsAfterOffers1\n-------")

    val itemCountsAfterOffers2 = itemCountsAfterOffers1.map((k, v) => (k, products(k).offer2.calcUnitsWithOffer2(itemCountsAfterOffers1, products, k)))
    println(s"itemCountsAfterOffers2= $itemCountsAfterOffers2\n-------")

    val itemTotalsAfterOffers = itemCountsAfterOffers2.map((key, quantity) => (key, products(key).price * quantity))
    println(s"${itemCounts.mkString("[", ",", "]")} => ${itemCountsAfterOffers2.mkString("[", ",", "]")} => $itemTotalsAfterOffers")

    val sumItemTotals = itemTotalsAfterOffers.foldLeft(BigDecimal("0.00")) { case (a, (_, v)) => a + v }
    println(s"£$sumItemTotals")
    sumItemTotals
  }


  def main(args: Array[String]): Unit = {
    val products = Map(
      "apple" -> Item("apple", price = 0.60, BuyOneGetOneFree(), OfferCheapestFree("apple", "banana")), // OfferCheapestFree needs to be applied to both products in case the price changes, otherwise just apply it to the cheapest and assume the price never changes
      "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree(), NoOffer2()),
      "banana" -> Item("banana", price = 0.20, NoOffer(), OfferCheapestFree("apple", "banana"))
    )

    println("Welcome to the ShopCart app!!")
    val items: List[String] = List(" Apple ", "APPLE", "Orange", "Apple", "", "InVaLiD", "banana", "BANANA", " banana ")
    println(s"items are : ${items.mkString(", ")}\n")
    calcTotal(items, products)
  }
}


