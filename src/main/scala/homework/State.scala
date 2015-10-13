package homework

case class State[S,+A](run: S => (A, S)) { self =>
  def map[B](f: A => B): State[S,B] = State { (s1: S) =>
    val (a, s2) = run(s1)
    (f(a), s2)
  }

  def map2[B,C](rb: State[S,B])(f: (A, B) => C): State[S,C] = State { (s1: S) =>
    val (a, s2) = run(s1)
    val (b, s3) = rb.run(s2)
    (f(a, b), s3)
  }

  def flatMap[B](f: A => State[S,B]): State[S,B] = State { (s1: S) =>
    val (x, s2) = run(s1)
    f(x).run(s2)
  }

  def both[B](rb: State[S,B]): State[S,(A,B)] = State { (s1: S) =>
    self.map2(rb)((a, b) => (a, b)).run(s1)
  }
}

object State {
  def unit[S,A](x: A): State[S,A] = State { (s: S) => (x, s) }

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = State { (s: S) =>
    fs.foldRight((List[A](), s)) { (f, acc) =>
      acc match {
        case (xs, s2) => f.run(s2) match {
          case (x, s3) => (x::xs, s3)
        }
      }
    }
  }
}
