package homework

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{ arbitrary }

import homework.RNG._

class RNGProps extends HomeworkPropertyCheck {
  implicit def arbRNG: Arbitrary[RNG] = Arbitrary(arbitrary[Long].map(SimpleRNG(_)))

  case class TestRNG(const: Int) extends RNG {
    def nextInt = {
      (const, TestRNG(const))
    }
  }

  "nonNegativeInt" should {
    "generate an Int >= 0" in {
      prop { r: RNG =>
        val (m, rng) = nonNegativeInt.run(r)
        m >= 0
      }
    }
  }

  "double" should {
    "generate 0 >= n < 1" in {
      prop { r: RNG =>
        val (n, rng) = double.run(r)
        0 <= n && n < 1
      }
    }
  }

  "sequence" should {
    //"result in a new Rand[Int] whose result is the same length as the List of computations" in {
      //prop { (rng: RNG, rs: List[State[RNG,Int]]) =>
        //val (ns, rng2) = sequence(rs).run(rng)
        //ns.length == rs.length
      //}
    //}

    "behave the same as map2 for a List of Rand of size 2" in {
      val rng = TestRNG(Int.MaxValue)
      double.map2(double)((l, r) => l::r::Nil).run(rng) mustEqual sequence(double::double::Nil).run(rng)
    }
  }
}
