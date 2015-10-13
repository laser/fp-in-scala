package homework

import org.specs2.mutable.Specification

import homework.Machine._

class MachineSpec extends Specification {
  "inserting a coin" should {
    "into a locked machine unlocks it if there is any candy left" in {
      val s1 = Machine(locked=true, candies=1, coins=0)
      val (r, s2) = handle(Coin).run(s1)

      s2 mustEqual Machine(locked=false, candies=1, coins=1)
    }

    "into an unlocked machine does nothing" in {
      val s1 = Machine(locked=false, candies=1, coins=0)
      val (r, s2) = handle(Coin).run(s1)

      s2 mustEqual Machine(locked=false, candies=1, coins=1)
    }
  }

  "turning the knob" should {
    "on an unlocked machine will cause it to dispense candy and become locked" in {
      val s1 = Machine(locked=false, candies=1, coins=0)
      val (r, s2) = handle(Turn).run(s1)

      s2 mustEqual Machine(locked=true, candies=0, coins=0)
    }

    "on a locked machine does nothing" in {
      val s1 = Machine(locked=true, candies=1, coins=0)
      val (r, s2) = handle(Turn).run(s1)

      s2 mustEqual Machine(locked=true, candies=1, coins=0)
    }
  }

  "simulating" should {
    "run a List[Input] seqentially" in {
      val s1 = Machine(locked=true, candies=5, coins=10)
      val is = Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Nil
      val (r, s2) = simulateMachine(is).run(s1)

      r mustEqual (14, 1)
    }
  }
}
