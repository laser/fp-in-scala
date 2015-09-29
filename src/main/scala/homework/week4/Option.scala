package homework

sealed trait Option[+A] { self =>
  def map[B](f: A => B): Option[B] = self match {
    case Just(x) => Just(f(x))
    case _ => Nothing
  }

  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case Just(x) => f(x)
    case _ => Nothing
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Just(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = self match {
    case Just(x) => Just(x)
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = self match {
    case Just(x) if f(x) => self
    case _ => Nothing
  }
}
case class Just[+A](get: A) extends Option[A]
case object Nothing extends Option[Nothing]

object Option {
  // Exercise 4.2 page 55
  def variance(xs: Seq[Double]): Option[Double] = ???

  // Exercise 4.3, page 58
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Just(x), Just(y)) => Just(f(x, y))
    case _ => Nothing
  }

  // Exercise 4.4, page 59
  def sequence[A](mas: List[Option[A]]): Option[List[A]] = mas match {
    case Nil    => Nothing
    case x::Nil => x.map { x2 => List[A](x2) }
    case x::xs  => map2(x, sequence(xs))(_::_)
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil    => Nothing
    case x::Nil => f(x).map { x2 => List[B](x2) }
    case x::xs  => map2(f(x), traverse(xs)(f))(_::_)
  }

  // Exercise 4.5, page 59
  def sequenceViaTraverse[A](mas: List[Option[A]]): Option[List[A]] = traverse(mas)(id)
}
