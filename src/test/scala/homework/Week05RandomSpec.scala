package homework

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{ arbitrary }

import homework.RNG._

class RNGProps extends HomeworkPropertyCheck {
  implicit def arbRNG: Arbitrary[RNG] = Arbitrary(arbitrary[Long].map(SimpleRNG(_)))

  "nonNegativeInt" should {
    "generate an Int >= 0" in {
      prop { r: RNG =>
        val (n, rng) = nonNegativeInt(r)
        0 <= n
      }
    }
  }

  "double" should {
    "generate 0 >= n < 1" in {
      prop { r: RNG =>
        val (n, rng) = double(r)
        0 <= n && n < 1
      }
    }
  }
}
