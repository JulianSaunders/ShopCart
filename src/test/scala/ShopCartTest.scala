
import org.scalatest.funsuite.AnyFunSuite

class ShopCartTest extends AnyFunSuite:

  test("should return 0.00 if no items") {
    val result = ShopCart.calcTotal(List.empty)
    assert(result == 0.00)
  }

  test("should return 0.00 if blank item") {
    val result = ShopCart.calcTotal(List(""))
    assert(result == 0.00)
  }

  test("should return 0.0 for INVALID item") {
    val result = ShopCart.calcTotal(List("INVALID"))
    assert(result == 0.00)
  }

  test("should return 0.60 for ['apple'] as lowercase") {
    val result = ShopCart.calcTotal(List("apple"))
    assert(result == 0.60)
  }

  test("should return 0.60 for ['  apple  '] with extra spaces") {
    val result = ShopCart.calcTotal(List("  apple  "))
    assert(result == 0.60)
  }
  
  test("should return 0.60 for ['Apple'] as Camel case") {
    val result = ShopCart.calcTotal(List("Apple"))
    assert(result == 0.60)
  }

  test("should return 0.60 for ['APPLE'] as UPPER CASE") {
    val result = ShopCart.calcTotal(List("APPLE"))
    assert(result == 0.60)
  }

  test("should return 0.6 for ['Apple', 'Apple'] with BuyOneGetOneFree offer applied, 1 free apple") {
    val result = ShopCart.calcTotal(List.fill(2)("Apple"))
    assert(result == 0.6)
  }

  test("should return 1.20 for ['Apple', 'Apple', 'Apple'] with BuyOneGetOneFree offer applied, 1 free apple") {
    val result = ShopCart.calcTotal(List.fill(3)("Apple"))
    assert(result == 1.20)
  }

  test("should return 1.20 for ['Apple', 'Apple', 'Apple', 'Apple'] with BuyOneGetOneFree offer applied, 2 free apples") {
    val result = ShopCart.calcTotal(List.fill(4)("Apple"))
    assert(result == 1.20)
  }

  test("should return 0.25 for ['Orange']") {
    val result = ShopCart.calcTotal(List("Orange"))
    assert(result == 0.25)
  }

  test("should return 0.50 for ['Orange', 'Orange']") {
    val result = ShopCart.calcTotal(List.fill(2)("Orange"))
    assert(result == 0.50)
  }

  test("should return 0.50 for ['Orange', 'Orange', 'Orange'] with BuyTwoGetOneFree offer applied, 1 free orange") {
    val result = ShopCart.calcTotal(List.fill(3)("Orange"))
    assert(result == 0.50)
  }

  test("should return 1.45 for ['Apple', 'Apple', 'Orange', 'Apple'] with BuyOneGetOneFree offer applied") {
    val result = ShopCart.calcTotal(List("Apple", "Apple", "Orange", "Apple"))
    assert(result == 1.45)
  }

  test("should return 1.95 for ['Apple', 'Apple', 'Orange', 'Apple', 'Orange', 'Orange', 'Orange'] with BuyOneGetOneFree offer applied AND BuyTwoGetOneFree offer applied") {
    val result = ShopCart.calcTotal(List("Apple", "Apple", "Orange", "Apple", "Orange", "Orange", "Orange"))
    assert(result == 1.95)
  }

  test("should return 3.05 for ['INVALID', 'Apple', 'Apple', 'Orange', 'Orange', 'Apple', 'Apple', 'Apple', 'Orange', 'Orange', 'Orange', 'Orange', 'Orange', 'InVaLiD']") {
    val result = ShopCart.calcTotal(List("INVALID", "Apple", "Apple", "Orange", "Orange", "Apple", "Apple", "Apple", "Orange", "Orange", "Orange", "Orange", "Orange", "InVaLiD"))
    assert(result == 3.05)
  }

end ShopCartTest