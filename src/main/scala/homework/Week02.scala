package homework

import scala.annotation.tailrec

object Week02 {

  // Exercise 2.1, page 21
  def fib(n: Int): Int = {
    @tailrec
    def go(n1: Int, n2: Int, iterations: Int): Int = {
      if (iterations > n) n1+n2
      else go(n2, n1+n2, iterations+1)
    }

    n match {
      case _ if (n < 1) => throw new Error("Not defined")
      case 1 => 0
      case 2 => 1
      case _ => go(0, 1, 3)
    }
  }

  // Exercise 2.2, page 24
  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(acc: Boolean, as: List[A]): Boolean = {
      as match {
        case x::y::ys => go(acc && ordered(x, y), y::ys)
        case _ => acc
      }
    }

    go(true, as)
  }

  // Exercise 2.3, page 27
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (x: A) => (y: B) => f(x, y)

  // Exercise 2.4, page 27
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (x: A, y: B) => f(x)(y)

  // Exercise 2.5, page 27
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (x: A) => f(g(x))
}
