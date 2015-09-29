package homework

sealed trait Maybe[+A] { self =>
  def map[B](f: A => B): Maybe[B] = self match {
    case Just(x) => Just(f(x))
    case _ => Nothing
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = self match {
    case Just(x) => f(x)
    case _ => Nothing
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Just(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = self match {
    case Just(x) => Just(x)
    case _ => ob
  }

  def filter(f: A => Boolean): Maybe[A] = self match {
    case Just(x) if f(x) => self
    case _ => Nothing
  }
}
case class Just[+A](get: A) extends Maybe[A]
case object Nothing extends Maybe[Nothing]

object Maybe {
  // Exercise 4.2 page 55
  def variance(xs: Seq[Double]): Maybe[Double] = ???

  // Exercise 4.3, page 58
  def map2[A,B,C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] = (a, b) match {
    case (Just(x), Just(y)) => Just(f(x, y))
    case _ => Nothing
  }

  // Exercise 4.4, page 59
  def sequence[A](mas: List[Maybe[A]]): Maybe[List[A]] = mas match {
    case Nil    => Nothing
    case x::Nil => x.map { x2 => List[A](x2) }
    case x::xs  => map2(x, sequence(xs))(_::_)
  }

  def traverse[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] = as match {
    case Nil    => Nothing
    case x::Nil => f(x).map { x2 => List[B](x2) }
    case x::xs  => map2(f(x), traverse(xs)(f))(_::_)
  }

  // Exercise 4.5, page 59
  def sequenceViaTraverse[A](mas: List[Maybe[A]]): Maybe[List[A]] = traverse(mas)(id)
}
