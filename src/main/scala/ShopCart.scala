// I've left the println's in so you can see what it is doing. Obviously they can be removed or swapped for a logger

object ShopCart {

  private class Item(val price: BigDecimal)

  private val products = Map(
    "apple" -> Item(price = 0.60),
    "orange" -> Item(price = 0.25)
  )

  def calcTotal(items: List[String]): BigDecimal = {
    val itemPriceTotals = items.map(_.trim.toLowerCase).filter(products.isDefinedAt).groupMapReduce(identity)(b => products(b).price)(_ + _) // I could make this function shorter by appending ".values.sum" to this line
    println(s"---\nitemPriceTotals= $itemPriceTotals")

    val total = itemPriceTotals.values.sum
    println(s"total= £$total")
    total
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to the ShopCart app!!")
    val items: List[String] = List(" Apple ", "APPLE", "Orange", "Apple", "", "InVaLiD")
    println(s"items are : ${items.mkString(", ")}\n")
    calcTotal(items)
  }
}


