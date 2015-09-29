package homework

import scala.{ Option => _, Some => _, None => _ }

import org.specs2.mutable._

import homework.Option._

object OptionSpec extends Specification {

  val something: Option[Int] = Some(10)
  val nothing: Option[Int]  = None

  "map" should {
    ".. work?" in {
      something.map(_+5) mustEqual Some(15)
      nothing.map(_+5) mustEqual nothing
    }
  }

  "flatMap" should {
    ".. work?" in {
      something.flatMap { x =>
        Some(x+5)
      } mustEqual Some(15)
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
      something.orElse(Some(999)) mustEqual something
      nothing.orElse(nothing) mustEqual nothing
      nothing.orElse(something) mustEqual something
    }
  }

  "filter" should {
    ".. work?" in {
      something.filter { x => x == 10 } mustEqual Some(10)
      something.filter { x => x == 11 } mustEqual None
      nothing.filter(Function.const(true)) mustEqual None
    }
  }

  "sequence" should {
    ".. work?" in {
      val mas1 = List(Some(1), None)
      val mas2 = List(None, Some(1))
      val mas3 = List(Some(1), Some(2), Some(3))

      sequence(mas1) mustEqual None
      sequence(mas2) mustEqual None
      sequence(mas3) mustEqual Some(List(1,2,3))
    }
  }

  "traverse" should {
    ".. work?" in {
      val xs = List(1,2,3,4,5)
      val p = (x: Int) => if (x > 4) Some(x) else None

      traverse(xs)(p) mustEqual None
      traverse(xs)(Some(_)) mustEqual Some(List(1,2,3,4,5))
    }
  }
}
