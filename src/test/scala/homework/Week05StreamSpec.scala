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

  "mapViaUnfold" should {
    ".. work?" in {
      fibs.take(5).mapViaUnfold(_+1).toList mustEqual List(1,2,2,3,4)
    }
  }

  "zipWithViaUnfold" should {
    ".. work?" in {
      fibs.zipWithViaUnfold(fibs)(_+_).take(5).toList mustEqual List(0, 2, 2, 4, 6)
    }
  }

  "startsWith" should {
    ".. tell me if a Stream starts with some other Stream" in {
      constant(5).startsWith(Stream(1)) mustEqual false
      constant(5).startsWith(Stream(5)) mustEqual true
      Stream(5).startsWith(constant(5)) mustEqual false
      Stream(5, 5, 5).startsWith(Stream(5, 5)) mustEqual true
    }
  }

  "tails" should {
    ".. return the Stream of suffixes of the input sequence" in {
      Stream(1,2,3).tails.map(_.toList).toList mustEqual List(List(1,2,3), List(2,3), List(3), Nil)
    }
  }

  "hasSubsequence" should {
    ".. tell you if a Stream contains a subsequence (Stream)" in {
      fibs.take(10).hasSubsequence(Stream(0, 99)) mustEqual false
      fibs.take(10).hasSubsequence(Stream(2, 3, 5)) mustEqual true
    }
  }

  "scanRight" should {
    ".. create a new Stream of intermediate results" in {
      Stream(1,2,3).scanRight(0)(_+_).toList mustEqual List(6,5,3,0)
      Stream(1,2,3).scanRight("0")((item, acc) => s"$item+$acc").toList mustEqual List("1+2+3+0", "2+3+0", "3+0", "0")
    }
  }
}
