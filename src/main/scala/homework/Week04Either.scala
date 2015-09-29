package homework

// Exercise 4.6, page 62
sealed trait Either[+E, +A] { self =>
  def map[B](f: A => B): Either[E, B] = self match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
    case Right(x) => Right(x)
    case Left(x) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (self, b) match {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(x), _)         => Left(x)
    case (_, Left(x))         => Left(x)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7, page 62
  //def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  //def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  // Exercise 4.8, page 63
  //
  // how would sequence and traverse need to change in order to validate everything
  // and collect errors?
}
