package homework

import org.specs2.mutable._

import homework.Stream._

object Week05StreamSpec extends Specification {
  "headOption" should {
    ".." in {
      val s1 = Empty:Stream[Int]
      val s2 = Stream(1,2,3)

      s1.headOption mustEqual None
      s2.headOption mustEqual Some(1)
    }
  }

  "take" should {
    ".." in {
      val s1 = Stream(1,2,3,4,5,6)
      s1.take(3).toList mustEqual List(1,2,3)

      val s2 = Empty
      s2.take(10).toList mustEqual List()
    }
  }

  "drop" should {
    ".." in {
      val s1 = Stream(1,2,3,4,5,6,7)
      s1.drop(2).toList mustEqual List(3,4,5,6,7)

      val s2 = Empty
      s2.drop(10).toList mustEqual List()

      val s3 = constant(1)
      s3.drop(1).take(4).toList mustEqual List(1,1,1,1)
    }
  }

  "takeWhile" should {
    ".." in {
      val expensive = (x: Int) => () => { println(x); x }

      val s1 = Stream(expensive(1), expensive(2), expensive(3), expensive(4), expensive(5))

      s1.takeWhile(f => f() < 2).toList.map(_()) mustEqual List(1)
    }
  }

  "generate" should {
    ".." in {
      val s1 = generate(1)(_+1)

      s1.take(5).toList mustEqual List(1,2,3,4,5)
    }
  }

  "foldRight" should {
    ".." in {
      val s1 = constant(true)
      s1.foldRight(false)(_||_) mustEqual true
    }
  }

  "forAll" should {
    ".." in {
      val s1 = Stream(1,2,3,4,5)
      s1.forAll(_ > 0) mustEqual true
    }
  }

  "filter" should {
    ".." in {
      val s1 = Stream(1,2,3,4,5)
      s1.filter(_ < 4).toList mustEqual List(1,2,3)
    }
  }

  "fibs" should {
    ".. not blow the stack" in {
      fibs.take(5).toList mustEqual List(0, 1, 1, 2, 3)
    }
  }
}
