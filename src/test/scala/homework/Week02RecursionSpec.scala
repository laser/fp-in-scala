package homework

import org.specs2.mutable._

import homework.Recursion.{ fib, isSorted, curry, uncurry, compose }

object Week02Spec extends Specification {
  "fibs" should {
    "correctly compute the nth number in the Fibonacci sequence" in {
      fib(1) mustEqual 0
      fib(2) mustEqual 1
      fib(3) mustEqual 1
      fib(4) mustEqual 2
      fib(5) mustEqual 3
      fib(6) mustEqual 5
    }
  }

  "isSorted" should {
    "sort a List[A] given a comparison function operating over (A, A)" in {
      val lt = (x: Int, y: Int) => x < y

      isSorted(List[Int](1,2,3,4))(lt) mustEqual true
      isSorted(List[Int](2,2,3,4))(lt) mustEqual false
      isSorted(List[Int](1,3,2,4))(lt) mustEqual false
    }
  }

  "curry" should {
    "return A => B => C given (A, B) => C" in {
      val product = (x: Int, y: Int) => x * y
      val curriedProduct = curry(product)

      curriedProduct(10)(50) mustEqual 500
    }
  }

  "uncurry" should {
    "return (A, B) => C given A => B => C" in {
      val curriedProduct = (x: Int) => (y: Int) => x * y
      val product = uncurry(curriedProduct)

      product(10, 50) mustEqual 500
    }
  }

  "compose" should {
    "return A => C given ((B => C), (A => B))" in {
      val f = (x: Int) => x / 2
      val g = (x: Int) => x + 10
      val fog = compose(f, g)

      fog(10) mustEqual 10
    }
  }
}
