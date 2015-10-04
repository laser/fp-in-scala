package homework

import scala.annotation.tailrec

import homework.Stream._

sealed trait Stream[+A] { self =>
  //def headOption: Option[A] = self match {
    //case Empty => None
    //case Cons(h, t) => Some(h())
  //}

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
  def take(n: Int): Stream[A] =  self match {
    case Empty => Empty
    case Cons(_, _) if (n < 1) => Empty
    case Cons(h, t) => cons(h(), t().take(n-1))
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

  // Exercise 5.3
  // see 5.5 below
  //def takeWhile(p: A => Boolean): Stream[A] = self match {
    //case Empty => Empty
    //case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p))
                       //else Empty
  //}

  def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = self.foldRight(true)(p(_) && _)

  def exists(p: A => Boolean): Boolean = self.foldRight(false)(p(_) || _)

  // Exercise 5.5
  def takeWhile(p: A => Boolean): Stream[A] = self.foldRight(Empty:Stream[A]){ (item, acc) =>
    if (p(item)) cons(item, acc) else Empty
  }

  // Exercise 5.6
  def headOption: Option[A] = self.foldRight(None:Option[A])((x, _) => Some(x))

  // Exercise 5.7
  // map, filter, append, and flatMap using foldRight
  def map[B](f: A => B): Stream[B] =
    self.foldRight(Empty:Stream[B])((x, z) => cons(f(x), z))

  def filter(p: A => Boolean): Stream[A] =
    self.foldRight(Empty:Stream[A])((x, z) => if (p(x)) cons(x, z) else z)

  def append[B>:A](x: => B): Stream[B] =
    self.foldRight(cons(x, empty))((x, z) => cons(x, z))

  def concat[B>:A](s: => Stream[B]): Stream[B] =
    self.foldRight(s)((x, z) => cons(x, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    self.foldRight(empty[B])((x, z) => f(x).foldRight(empty[B])((x2, z2) => cons(x2, z2)))

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(self)({
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  })

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, self))({
    case (_, Empty) => None
    case (m, Cons(_, _)) if m == 0 => None
    case (m, Cons(h, t)) => Some((h(), (m-1, t())))
  })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(self)({
    case Empty => None
    case Cons(h, _) if !p(h()) => None
    case Cons(h, t) => Some((h(), t()))
  })

  def zipWithViaUnfold[B>:A](as: Stream[B])(f: (B, B) => B): Stream[B] = unfold((self, as))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _                            => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((self, s2))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), _)            => Some(((Some(h1()), None), (t1(), empty)))
    case (_, Cons(h2, t2))            => Some(((None, Some(h2())), (empty, t2())))
    case _                            => None
  })

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean = self.zipAll(s).foldRight(true)({ (item, z) =>
    item match {
      case (Some(x), Some(y)) => x == y && z
      case (Some(_), None) => true // laziness ftw
      case _ => false
    }
  })

  // Exercise 5.15
  def tails: Stream[Stream[A]] = unfold(self)({
    case s@Cons(h, t) => Some((s, t()))
    case Empty => None
  }).append(empty[A])

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))

  // Exercise 5.16

  // Note: scanRight didn't work because I would have computed f over the whole
  //       stream on the first shot and there was no way to unwind as we moved
  //       towards the head of the original Stream
  //
  //def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = unfold((z, self))({
    //case (z2, s@Cons(h, t)) => Some((s.foldRight(z2)(f), (z2, t())))
    //case (z2, Empty) => None
  //}).append(z)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    self.foldRight((z, cons(z, empty[B])))({ (item, acc) => acc match {
      case (z2, s) => (f(item, z2), cons(f(item, z2), s))
    }})._2
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

  // Exercise 5.8 (see 5.12)
  //def constant[A](a: A): Stream[A] = {
    //lazy val tail: Stream[A] = Cons(() => a, () => tail)
    //tail
  //}

  def generate[A](seed: A)(next: A => A): Stream[A] = cons(seed, generate(next(seed))(next))

  // Exercise 5.9 (see 5.12)
  //def from(n: Int): Stream[Int] = generate(n)(_+1)

  // Exercise 5.10 (see 5.12)
  //def fibs: Stream[Int] = {
    //def generate2(seed: (Int, Int))(next: (Int, Int) => (Int, Int)): Stream[(Int, Int)] =
      //cons(seed, generate2(next.tupled(seed))(next))

    //generate2(0, 1)((x, y) => (y, x+y)).map(_._1)
  //}

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((x, z2)) => cons(x, unfold(z2)(f))
  }

  // Exercise 5.12
  def fibs: Stream[Int] =
    Stream(0, 1).concat(unfold((0, 1))((s) => Some((s._1+s._2, (s._2, s._1+s._2)))))

  def from(n: Int): Stream[Int] =
    unfold(n)((s) => Some(s, s+1))

  def constant[A](x: A): Stream[A] = unfold(x)((s) => Some(s, s))

  def ones: Stream[Int] = unfold(1)(Function.const(Some(1, 1)))

}
