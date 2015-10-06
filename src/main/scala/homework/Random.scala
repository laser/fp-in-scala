package homework

import homework.RNG._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  val int: Rand[Int] = _.nextInt

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt

    if (n < 0) ((n+1) * -1, rng2)
    else       (n, rng2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, rng2) => (n.toDouble / Int.MaxValue.toDouble, rng2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = rng.nextInt match {
    case (n, rng2) => double(rng2) match {
      case (n2, rng3) => ((n, n2), rng3)
    }
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((n1, n2), rng2) => ((n2, n1), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (n1, rng1) = double(rng)
    val (n2, rng2) = double(rng1)
    val (n3, rng3) = double(rng2)
    ((n1,n2,n3), rng3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(counter: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (counter == 0) (acc, rng)
      else {
        val (n1, rng1) = rng.nextInt
        go(counter-1, n1::acc, rng1)
      }

    go(count, Nil, rng)
  }

  // Exercise 6.5
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def doubleUsingMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(n => n.toDouble / Int.MaxValue.toDouble)(rng)

  // Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((a, b) => (a, b))

  def intDoubleUsingBoth(rng: RNG): ((Int, Double), RNG) = both(_.nextInt, double)(rng)

  def doubleIntUsingBoth(rng: RNG): ((Double, Int), RNG) = both(double, _.nextInt)(rng)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???
}
