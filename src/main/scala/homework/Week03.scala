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
      if (counter >= n) as
      else go(tail(as), counter+1)
    }

    go(as, 0)
  }

  // Exercise 3.5, page 36
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A]): List[A] = {
      as match {
        case x::xs if p(x) => go(xs)
        case _ => as
      }
    }

    go(as)
  }

  // Exercise 3.6, page 37
  def init[A](as: List[A]): List[A] = {
    as match {
      case x::y::ys => x :: init(y::ys)
      case _ => Nil
    }
  }

  // Exercise 3.7, page 40
  // Exercise 3.8, page 40
  // Exercise 3.9, page 40
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x::xs => f(x, foldRight(xs, z)(f)) 
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc: Int) => acc + 1)

  // Exercise 3.10, page 40
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x::xs => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11, page 41
  def sum(is: List[Int]): Int = foldLeft(is, 0)(_+_) 
  def product(is: List[Int]): Int = foldLeft(is, 1)(_*_)
  def length2(as: List[Int]): Int = foldLeft(as, 0)((acc: Int, _) => acc + 1)
  
  // Exercise 3.12, page 41
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc: List[A], item: A) => item :: acc)

  // Exercise 3.13, page 41
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???

  // Exercise 3.14, page 41
  def append[A](as1: List[A], as2: List[A]): List[A] = foldRight(as1, as2)((item: A, acc: List[A]) => item :: acc)

  // Exercise 3.15, page 41
  def flatten[A](lols: List[List[A]]): List[A] =
    foldRight(lols, List[A]())((item: List[A], acc: List[A]) => foldRight(item, acc)(_::_))

  // Exercise 3.16, page 42
  //def incrementEachByOne(is: List[Int]): List[Int] = foldRight(is, List[Int]())(_+1::_)
  def incrementEachByOne(is: List[Int]): List[Int] = map(is)(_+1)

  // Exercise 3.17, page 42
  //def doublesToStrings(ds: List[Double]): List[String] = foldRight(ds, List[String]())(_.toString::_)
  def doublesToStrings(ds: List[Double]): List[String] = map(ds)(_.toString)

  // Exercise 3.18, page 42
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())(f(_)::_)

  // Exercise 3.19, page 42
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, List[A]())((item: A, acc: List[A]) => if (p(item)) item::acc else acc)

  // Exercise 3.20, page 42
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((item: A, acc: List[B]) => foldRight(f(item), acc)(_::_))

  // Exercise 3.21, page 43
  def filterViaFlatMap[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as) { item =>
    if (p(item)) List(item)
    else Nil
  }

  // Exercise 3.22, page 43
  def zipWithAdd(is1: List[Int], is2: List[Int]): List[Int] = (is1, is2) match {
    case (x::xs, y::ys) => (x+y) :: zipWithAdd(xs, ys)
    case _              => List[Int]()
  }

  // Exercise 3.23, page 43
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as1, as2) match {
    case (x::xs, y::ys) => f(x, y) :: zipWith(xs, ys)(f)
    case _              => List[A]()
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // Exercise 3.24, page 44
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

  // Exercise 3.25, page 46
  def treeSize[A](t: Tree[A]): Int = ???

  // Exercise 3.26, page 46
  def intTreeMax(t: Tree[Int]): Int = ???

  // Exercise 3.27, page 46
  def treeDepth[A](t: Tree[A]): Int = ???

  // Exercise 3.28, page 46
  def treeMap[A, B](t: Tree[A])(f: A => B): Tree[B] = treeFold((v: A) => Leaf(f(v)): Tree[B])(Branch(_,_))(t)

  // Exercise 3.29, page 47
  def treeFold[A, B](l: A => B)(b: (B, B) => B)(t: Tree[A]): B = t match {
    case Leaf(value) => l(value)
    case Branch(left, right) => b(treeFold(l)(b)(left), treeFold(l)(b)(right))
  }
}
