// I've left the println's in so you can see what it is doing. They can be removed or swapped for a logger

object ShopCart {

  sealed trait Offer {
    def calcPriceWithOffer(quantity: Int, item: Item): BigDecimal
  }

  private class BuyOneGetOneFree extends Offer {
    override def calcPriceWithOffer(quantity: Int, item: Item): BigDecimal = {
      val redeemedOfferUnits = quantity / 2
      val remainderUnits = quantity - (redeemedOfferUnits * 2)
      val totalUnitsToBill = redeemedOfferUnits + remainderUnits
      if (redeemedOfferUnits > 0) println(s"BuyOneGetOneFree offer applied for '${item.sku}' quantity=$quantity  redeemedOfferUnits=$redeemedOfferUnits  remainderUnits=$remainderUnits  totalUnitsToBill=$totalUnitsToBill")
      totalUnitsToBill * item.price
    }
  }

  private class BuyTwoGetOneFree extends Offer {
    override def calcPriceWithOffer(quantity: Int, item: Item): BigDecimal = {
      val redeemedOfferCount = quantity / 3
      val remainderUnits = quantity - (redeemedOfferCount * 3)
      val totalUnitsToBill = (redeemedOfferCount * 2) + remainderUnits
      if (redeemedOfferCount > 0) println(s"BuyTwoGetOneFree offer applied for '${item.sku}'  quantity=$quantity  redeemedOfferCount=$redeemedOfferCount  remainderUnits=$remainderUnits  totalUnitsToBill=$totalUnitsToBill")
      totalUnitsToBill * item.price
    }
  }

  class Item(val sku: String, val price: BigDecimal, val offer: Offer)

  private val products = Map(
    "apple" -> Item("apple", price = 0.60, BuyOneGetOneFree()),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree())
  )

  def calcTotal(items: List[String]): BigDecimal = {
    val itemCounts = items.map(_.trim.toLowerCase).filter(products.isDefinedAt).groupMapReduce(identity)(_ => 1)(_ + _)
    println(s"-------\nitemCounts= $itemCounts")

    val sumOfItemTotalsWithOffers = itemCounts.map((key, quantity) => products(key).offer.calcPriceWithOffer(quantity, products(key))).sum
    println(s"${itemCounts.mkString("[", ",", "]")} => £$sumOfItemTotalsWithOffers")
    sumOfItemTotalsWithOffers
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to the ShopCart app!!")
    val items: List[String] = List(" Apple ", "APPLE", "Orange", "Apple", "", "InVaLiD")
    println(s"items are : ${items.mkString(", ")}\n")
    calcTotal(items)
  }
}


