
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

  test("should return 0.60 for ['apple']") {
    val result = ShopCart.calcTotal(List("apple"))
    assert(result == 0.60)
  }

  test("should return 0.60 for ['  apple  ']") {
    val result = ShopCart.calcTotal(List("  apple  "))
    assert(result == 0.60)
  }
  
  test("should return 0.60 for ['Apple']") {
    val result = ShopCart.calcTotal(List("Apple"))
    assert(result == 0.60)
  }

  test("should return 0.60 for ['APPLE']") {
    val result = ShopCart.calcTotal(List("APPLE"))
    assert(result == 0.60)
  }

  test("should return 0.25 for ['Orange']") {
    val result = ShopCart.calcTotal(List("Orange"))
    assert(result == 0.25)
  }

  test("should return 1.20 for ['Apple', 'Apple']") {
    val result = ShopCart.calcTotal(List.fill(2)("Apple"))
    assert(result == 1.20)
  }

  test("should return 0.50 for ['Orange', 'Orange']") {
    val result = ShopCart.calcTotal(List.fill(2)("Orange"))
    assert(result == 0.50)
  }

  test("should return 1.80 for ['Apple', 'Apple', 'Apple']") {
    val result = ShopCart.calcTotal(List.fill(3)("Apple"))
    assert(result == 1.80)
  }

  test("should return 0.50 for ['Orange', 'Orange', 'Orange']") {
    val result = ShopCart.calcTotal(List.fill(3)("Orange"))
    assert(result == 0.75)
  }

  test("should return 0.50 for ['Apple', 'Apple', 'Orange', 'Apple']") {
    val result = ShopCart.calcTotal(List("Apple", "Apple", "Orange", "Apple"))
    assert(result == 2.05)
  }

  test("should return 4.75 for ['INVALID', 'Apple', 'Apple', 'Orange', 'Orange', 'Apple', 'Apple', 'Apple', 'Orange', 'Orange', 'Orange', 'Orange', 'Orange', 'InVaLiD']") {
    val result = ShopCart.calcTotal(List("INVALID", "Apple", "Apple", "Orange", "Orange", "Apple", "Apple", "Apple", "Orange", "Orange", "Orange", "Orange", "Orange", "InVaLiD"))
    assert(result == 4.75)
  }

end ShopCartTest