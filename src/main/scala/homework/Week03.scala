package homework

import scala.annotation.tailrec

object Week03 {
  // Exercise 3.2, page 35
  def tail[A](as: List[A]): List[A] = {
    as match {
      case _::as => as
      case _ => Nil
    }
  }

  // Exercise 3.3, page 36
  def setHead[A](a: A, as: List[A]): List[A] = a::as

  // Exercise 3.4, page 36
  def drop[A](as: List[A], n: Int): List[A] = {
    @tailrec
    def go(as: List[A], counter: Int): List[A] = {
      if (counter > n) as
      else go(tail(as), counter+1)
    }

    go(as, 0)
  }

  // Exercise 3.5, page 36
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A], acc: Boolean): List[A] = {
      as match {
        case x::xs if acc => go(xs, p(x))
        case _ => as
      }
    }

    go(as, true)
  }

  // Exercise 3.6, page 37
  // Exercise 3.7, page 40
  // Exercise 3.8, page 40
  // Exercise 3.9, page 40
  // Exercise 3.10, page 40
  // Exercise 3.11, page 41
  // Exercise 3.12, page 41
  // Exercise 3.13, page 41
  // Exercise 3.14, page 41
  // Exercise 3.15, page 41
  // Exercise 3.16, page 42
  // Exercise 3.17, page 42
  // Exercise 3.18, page 42
  // Exercise 3.19, page 42
  // Exercise 3.20, page 42
  // Exercise 3.21, page 43
  // Exercise 3.22, page 43
  // Exercise 3.23, page 43
  // Exercise 3.24, page 44
  // Exercise 3.25, page 46
  // Exercise 3.26, page 46
  // Exercise 3.27, page 46
  // Exercise 3.28, page 46
  // Exercise 3.29, page 47
}
