
import ShopCart.{BuyOneGetOneFree, BuyTwoGetOneFree, Item, NoOffer, NoOffer2, OfferCheapestFree}
import org.scalatest.funsuite.AnyFunSuite

class ShopCartTest extends AnyFunSuite:

  val products1 = Map(
    "apple" -> Item("apple", price = 0.60, BuyOneGetOneFree(), NoOffer2()),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree(), NoOffer2())
  )

  val products2_bananaCheapest = Map(
    "apple" -> Item("apple", price = 0.60, BuyOneGetOneFree(), NoOffer2()),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree(), NoOffer2()),
    "banana" -> Item("banana", price = 0.20, NoOffer(), OfferCheapestFree("apple", "banana"))
  )

  val products3_appleCheapest = Map(
    "apple" -> Item("apple", price = 0.10, BuyOneGetOneFree(), OfferCheapestFree("apple", "banana")),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree(), NoOffer2()),
    "banana" -> Item("banana", price = 0.20, NoOffer(), NoOffer2())
  )

  val products4_appleSamePriceAsBanana = Map(
    "apple" -> Item("apple", price = 0.20, BuyOneGetOneFree(), NoOffer2()),
    "orange" -> Item("orange", price = 0.25, BuyTwoGetOneFree(), NoOffer2()),
    "banana" -> Item("banana", price = 0.20, NoOffer(), OfferCheapestFree("apple", "banana"))
  )

  test("should return 0.00 if no items") {
    val result = ShopCart.calcTotal(List.empty, products1)
    assert(result == 0.00)
  }

  test("should return 0.00 if blank item") {
    val result = ShopCart.calcTotal(List(""), products1)
    assert(result == 0.00)
  }

  test("should return 0.0 for INVALID item") {
    val result = ShopCart.calcTotal(List("INVALID"), products1)
    assert(result == 0.00)
  }

  test("should return 0.60 for ['apple'] as lowercase") {
    val result = ShopCart.calcTotal(List("apple"), products1)
    assert(result == 0.60)
  }

  test("should return 0.60 for ['  apple  '] with extra spaces") {
    val result = ShopCart.calcTotal(List("  apple  "), products1)
    assert(result == 0.60)
  }
  
  test("should return 0.60 for ['Apple'] as Camel case") {
    val result = ShopCart.calcTotal(List("Apple"), products1)
    assert(result == 0.60)
  }

  test("should return 0.60 for ['APPLE'] as UPPER CASE") {
    val result = ShopCart.calcTotal(List("APPLE"), products1)
    assert(result == 0.60)
  }

  test("should return 0.6 for ['Apple', 'Apple'] with BuyOneGetOneFree offer applied, 1 free apple") {
    val result = ShopCart.calcTotal(List.fill(2)("Apple"), products1)
    assert(result == 0.6)
  }

  test("should return 1.20 for ['Apple', 'Apple', 'Apple'] with BuyOneGetOneFree offer applied, 1 free apple") {
    val result = ShopCart.calcTotal(List.fill(3)("Apple"), products1)
    assert(result == 1.20)
  }

  test("should return 1.20 for ['Apple', 'Apple', 'Apple', 'Apple'] with BuyOneGetOneFree offer applied, 2 free apples") {
    val result = ShopCart.calcTotal(List.fill(4)("Apple"), products1)
    assert(result == 1.20)
  }

  test("should return 0.25 for ['Orange']") {
    val result = ShopCart.calcTotal(List("Orange"), products1)
    assert(result == 0.25)
  }

  test("should return 0.50 for ['Orange', 'Orange']") {
    val result = ShopCart.calcTotal(List.fill(2)("Orange"), products1)
    assert(result == 0.50)
  }

  test("should return 0.50 for ['Orange', 'Orange', 'Orange'] with BuyTwoGetOneFree offer applied, 1 free orange") {
    val result = ShopCart.calcTotal(List.fill(3)("Orange"), products1)
    assert(result == 0.50)
  }

  test("should return 1.45 for ['Apple', 'Apple', 'Orange', 'Apple'] with BuyOneGetOneFree offer applied") {
    val result = ShopCart.calcTotal(List("Apple", "Apple", "Orange", "Apple"), products1)
    assert(result == 1.45)
  }

  test("should return 1.95 for ['Apple', 'Apple', 'Orange', 'Apple', 'Orange', 'Orange', 'Orange'] with BuyOneGetOneFree offer applied AND BuyTwoGetOneFree offer applied") {
    val result = ShopCart.calcTotal(List("Apple", "Apple", "Orange", "Apple", "Orange", "Orange", "Orange"), products1)
    assert(result == 1.95)
  }

  test("should return 3.05 for ['INVALID', 'Apple', 'Apple', 'Orange', 'Orange', 'Apple', 'Apple', 'Apple', 'Orange', 'Orange', 'Orange', 'Orange', 'Orange', 'InVaLiD']") {
    val result = ShopCart.calcTotal(List("INVALID", "Apple", "Apple", "Orange", "Orange", "Apple", "Apple", "Apple", "Orange", "Orange", "Orange", "Orange", "Orange", "InVaLiD"), products1)
    assert(result == 3.05)
  }

  test("should return 0.20 for ['banana'] as lowercase") {
    val result = ShopCart.calcTotal(List("banana"), products2_bananaCheapest)
    assert(result == 0.20)
  }

  test("should return 0.60 for ['apple', 'banana'] as lowercase") {
    val result = ShopCart.calcTotal(List("apple", "banana"), products2_bananaCheapest)
    assert(result == 0.60)
  }

  test("should return 1.00 for ['apple', 'banana', 'banana', 'banana'] as lowercase") {
    val result = ShopCart.calcTotal(List("apple", "banana", "banana", "banana"), products2_bananaCheapest)
    assert(result == 1.00)
  }

  test("should return 1.00 for ['apple', 'apple', 'banana', 'banana', 'banana'] as lowercase") {
    val result = ShopCart.calcTotal(List("apple", "apple", "banana", "banana", "banana"), products2_bananaCheapest)
    assert(result == 1.00)
  }

  test("should return 1.40 for ['apple', 'apple', 'banana', 'banana', 'banana'] as lowercase") {
    val result = ShopCart.calcTotal(List.fill(3)("apple") ::: List("banana", "banana", "banana"), products2_bananaCheapest)
    assert(result == 1.40)
  }

  test("should return 0.60 for ['apple', 'apple', 'banana', 'banana', 'banana'] as lowercase") {
    val result = ShopCart.calcTotal(List.fill(3)("apple") ::: List("banana", "banana", "banana"), products3_appleCheapest)
    assert(result == 0.60)
  }

  test("should return 60 for 1000 X Apples, 100 X Banana as lowercase, apples cheapest") {
    val result = ShopCart.calcTotal(List.fill(1000)("apple") ::: List.fill(100)("banana"), products3_appleCheapest)
    assert(result == 60)
  }

  test("should return 300 for 1000 X Apples, 100 X Banana as lowercase, bananas cheapest") {
    val result = ShopCart.calcTotal(List.fill(1000)("apple") ::: List.fill(100)("banana"), products2_bananaCheapest)
    assert(result == 300)
  }

  test("should return 220 for 1000 X Apples, 100 X Banana as lowercase, bananas cheapest") {
    val result = ShopCart.calcTotal(List.fill(100)("apple") ::: List.fill(1000)("banana"), products2_bananaCheapest)
    assert(result == 30 + 190)
  }

  test("should return 210 for 1000 X Apples, 100 X Banana as lowercase, bananas cheapest") {
    val result = ShopCart.calcTotal(List.fill(100)("apple") ::: List.fill(1000)("banana"), products4_appleSamePriceAsBanana)
    assert(result == 10 + 200)
  }

end ShopCartTest