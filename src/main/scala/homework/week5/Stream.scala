package homework

import scala.annotation.tailrec

import homework.Stream._

sealed trait Stream[+A] { self =>
  def headOption: Option[A] = self match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = {
    @tailrec
    def go(acc: List[A], sas: Stream[A]): List[A] = sas match {
      case Empty => acc
      case Cons(h, t) => go(h()::acc, t())
    }

    go(Nil, self).reverse
  }

  // Exercise 5.2a
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(acc: Stream[A], counter: Int, as: Stream[A]): Stream[A] =
      if (counter == n) acc
      else as match {
        case Empty => acc
        case Cons(h, t) => go(cons(h(), acc), counter+1, t())
      }

    go(Empty, 0, self)
  }

  // Exercise 5.2b
  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(counter: Int, as: Stream[A]): Stream[A] =
      if (counter == n) as
      else as match {
        case Empty => as
        case Cons(h, t) => go(counter+1, t())
      }

    go(0, self)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
}
