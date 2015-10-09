package homework

import homework.State._
import homework.RNG._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt: State[RNG, Int] = State { (rng: RNG) =>
    val (n, rng2) = rng.nextInt

    if (n < 0) ((n+1) * -1, rng2)
    else       (n, rng2)
  }

  // Exercise 6.2
  def double: State[RNG, Double] = State { (rng: RNG) =>
    nonNegativeInt.run(rng) match {
      case (n, rng2) => (n.toDouble / Int.MaxValue.toDouble, rng2)
    }
  }

  // Exercise 6.3
  def intDouble: State[RNG, (Int, Double)] = State { (rng: RNG) =>
    rng.nextInt match {
      case (n, rng2) => double.run(rng2) match {
        case (n2, rng3) => ((n, n2), rng3)
      }
    }
  }

  def doubleInt: State[RNG, (Double, Int)] = State { (rng: RNG) =>
    intDouble.run(rng) match {
      case ((n1, n2), rng2) => ((n2, n1), rng2)
    }
  }

  def double3: State[RNG, (Double, Double, Double)] = State { (rng: RNG) =>
    val (n1, rng1) = double.run(rng)
    val (n2, rng2) = double.run(rng1)
    val (n3, rng3) = double.run(rng2)
    ((n1,n2,n3), rng3)
  }

  // Exercise 6.4
  def ints(count: Int): State[RNG, List[Int]] = State { (rng: RNG) =>
    def go(counter: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (counter == 0) (acc, rng)
      else {
        val (n1, rng1) = rng.nextInt
        go(counter-1, n1::acc, rng1)
      }

    go(count, Nil, rng)
  }

  def doubleUsingMap: State[RNG,Double] =
    State(nonNegativeInt.map(n => n.toDouble / Int.MaxValue.toDouble).run(_:RNG))

  def intDoubleUsingBoth: State[RNG,(Int, Double)] =
    State(State((rng: RNG) => rng.nextInt).both(double).run(_:RNG))

  def doubleIntUsingBoth: State[RNG,(Double, Int)] =
    State(double.both(State((rng: RNG) => rng.nextInt)).run(_:RNG))

  def nonNegativeLessThan(n: Int): State[RNG,Int] = nonNegativeInt.flatMap { m =>
    val mod = m % n
    if (m + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  // Exercise 6.7
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = State { (s: S) =>
    fs.foldRight((List[A](), s)) { (f, acc) =>
      acc match {
        case (xs, s2) => f.run(s2) match {
          case (x, s3) => (x::xs, s3)
        }
      }
    }
  }

  def mapViaFlatMap[S,A,B](s: State[S,A])(f: A => B): State[S,B] = s.flatMap(x => unit(f(x)))

  def map2ViaFlatMap[S,A,B,C](ra: State[S,A], rb: State[S,B])(f: (A, B) => C): State[S,C] = for {
    a <- ra
    b <- rb
  } yield f(a, b)
}
