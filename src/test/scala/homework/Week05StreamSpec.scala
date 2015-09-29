package homework

import org.specs2.mutable._

import homework.Stream._

object Week05StreamSpec extends Specification {
  "take" should {
    ".." in {
      val s1 = constant(1)
      s1.take(5).toList mustEqual List(1,1,1,1,1)

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
}
