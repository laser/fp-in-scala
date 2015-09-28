package homework 

import org.specs2.mutable._

import homework.Week04._

object Exercise4_1Spec extends Specification {

  val something: Maybe[Int] = Just(10)
  val nothing: Maybe[Int]  = Nothing

  "map" should {
    ".. work?" in {
      something.map(_+5) mustEqual Just(15)
      nothing.map(_+5) mustEqual nothing
    }
  }

  "flatMap" should {
    ".. work?" in {
      something.flatMap { x =>
        Just(x+5)
      } mustEqual Just(15)
    }
  }

  "getOrElse" should {
    ".. work?" in {
      something.getOrElse(0) mustEqual 10
      nothing.getOrElse(0) mustEqual 0
    }
  }

  "orElse" should {
    ".. work?" in {
      something.orElse(nothing) mustEqual something
      something.orElse(Just(999)) mustEqual something
      nothing.orElse(nothing) mustEqual nothing
      nothing.orElse(something) mustEqual something
    }
  }

  "filter" should {
    ".. work?" in {
      something.filter { x => x == 10 } mustEqual Just(10)
      something.filter { x => x == 11 } mustEqual Nothing
      nothing.filter(Function.const(true)) mustEqual Nothing
    }
  }
}

object Exercise4_4Spec extends Specification {
  "sequence" should {
    ".. work?" in {
      val mas1 = List(Just(1), Nothing)
      val mas2 = List(Nothing, Just(1))
      val mas3 = List(Just(1), Just(2), Just(3))

      sequence(mas1) mustEqual Nothing
      sequence(mas2) mustEqual Nothing
      sequence(mas3) mustEqual Just(List(1,2,3))
    }
  }

  "traverse" should {
    ".. work?" in {
      val xs = List(1,2,3,4,5)
      val p = (x: Int) => if (x > 4) Just(x) else Nothing

      traverse(xs)(p) mustEqual Nothing
      traverse(xs)(Just(_)) mustEqual Just(List(1,2,3,4,5))
    }
  }
}
