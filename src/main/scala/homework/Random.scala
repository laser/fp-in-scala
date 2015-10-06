package homework

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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt

    if (n < 0) ((n+1) * -1, rng2)
    else       (n, rng2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, rng2) => (n.toDouble / Int.MaxValue.toDouble, rng2)
  }
}
