// I've left the println's in so you can see what it is doing. They can be removed or swapped for a logger

object ShopCart {

  trait Offer {
    def calcOfferPrice(quantity: Int, item: Item): BigDecimal
  }

  private class BuyOneGetOneFree extends Offer {
    override def calcOfferPrice(quantity: Int, item: Item): BigDecimal = {
      val redeemedUnitsBilled = quantity / 2
      val remainderUnits = quantity - (redeemedUnitsBilled * 2)
      val totalUnitsBilled = redeemedUnitsBilled + remainderUnits
      if (redeemedUnitsBilled > 0) println(s"BuyOneGetOneFree offer applied for '${item.sku}' quantity=$quantity  redeemedUnitsBilled=$redeemedUnitsBilled  remainderUnits=$remainderUnits  totalUnitsBilled=$totalUnitsBilled")
      totalUnitsBilled * item.price
    }
  }

  private class BuyTwoGetOneFree extends Offer {
    override def calcOfferPrice(quantity: Int, item: Item): BigDecimal = {
      val redeemedCount = quantity / 3
      val remainderUnits = quantity - (redeemedCount * 3)
      val totalUnitsBilled = (redeemedCount * 2) + remainderUnits
      if (redeemedCount > 0) println(s"BuyTwoGetOneFree offer applied for '${item.sku}'  quantity=$quantity  redeemedCount=$redeemedCount  remainderUnits=$remainderUnits  totalUnitsBilled=$totalUnitsBilled")
      totalUnitsBilled * item.price
    }
  }

  class Item(val sku: String, val price: BigDecimal, val offer: Offer)

  private val products = Map(
    "apple" -> Item("apple", price = 0.60, BuyOneGetOneFree()),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree())
  )

  def calcTotal(items: List[String]): BigDecimal = {
    val itemCounts = items.map(_.trim.toLowerCase).filter(products.isDefinedAt).groupMapReduce(identity)(b => 1)(_ + _)
    println(s"-------\nitemCounts= $itemCounts")

    val sumOfItemTotalsWithOffers = itemCounts.map((key, quantity) => products(key).offer.calcOfferPrice(quantity, products(key))).sum
    println(s"sumOfItemTotalsWithOffers = $sumOfItemTotalsWithOffers")
    sumOfItemTotalsWithOffers
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to the ShopCart app!!")
    val items: List[String] = List(" Apple ", "APPLE", "Orange", "Apple", "", "InVaLiD")
    println(s"items are : ${items.mkString(", ")}\n")
    calcTotal(items)
  }
}


