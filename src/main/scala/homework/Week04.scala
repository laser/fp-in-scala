package homework

import scala.annotation.tailrec

object Week04 {

  def id[A](x: A) = x

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

  // Exercise 4.6, page 62
  sealed trait Either2[+E, +A] { self =>
    def map[B](f: A => B): Either2[E, B] = self match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

    def flatMap[EE >: E, B](f: A => Either2[EE, B]): Either2[EE, B] = self match {
      case Right(x) => f(x)
      case Left(x) => Left(x)
    }

    def orElse[EE >: E,B >: A](b: => Either2[EE, B]): Either2[EE, B] = self match {
      case Right(x) => Right(x)
      case Left(x) => b
    }

    def map2[EE >: E, B, C](b: Either2[EE, B])(f: (A, B) => C): Either2[EE, C] = (self, b) match {
      case (Right(x), Right(y)) => Right(f(x, y))
      case (Left(x), _)         => Left(x)
      case (_, Left(x))         => Left(x)
    }
  }

  case class Left[+E](value: E) extends Either2[E, Nothing] 
  case class Right[+A](value: A) extends Either2[Nothing, A]

  // Exercise 4.7, page 62
  //def sequence[E, A](es: List[Either2[E, A]]): Either2[E, List[A]] = ???

  //def traverse[E, A, B](as: List[A])(f: A => Either2[E, B]): Either2[E, List[B]] = ???

  // Exercise 4.8, page 63
  //
  // how would sequence and traverse need to change in order to validate everything
  // and collect errors?
}
