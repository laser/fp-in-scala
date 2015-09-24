package homework 

import org.scalacheck.Gen.{ listOfN, posNum }
import org.scalacheck.Prop.{ forAll }
import org.scalacheck.Properties
import org.specs2.mutable._
import org.specs2.ScalaCheck

import homework.Week03.{ tail, setHead, drop, dropWhile }

object G {
  val nonEmptyIntList = for {
    len <- posNum[Int]
    list <- listOfN(len, posNum[Int])
  } yield list
}

object ListTailProp extends Properties("tail") {
  property(".. of an arbitrary List[Int]") = forAll { (x: Int, xs: List[Int]) =>
    tail(x::xs) == xs
  }
}

object ListDropProp extends Properties("drop") {
  property(".. from empty list") = forAll(posNum[Int]) { n =>
    drop(List[Int](), n) == List[Int]()
  }

  property(".. more items than list contains") = forAll(posNum[Int], G.nonEmptyIntList) { (n, ns) =>
    drop(ns, ns.length+n) == List[Int]()
  }

  property(".. from non-empty list") = forAll(posNum[Int], G.nonEmptyIntList) { (n, ns) =>
    if (n > ns.length) drop(ns, n) == List[Int]()
    else drop(ns, n).length == ns.length-n
  }
}

object ListDropWhileProp extends Properties("dropWhile") {
  property(".. from empty list") = forAll { (f: Int => Boolean) =>
    dropWhile(List[Int](), f) == List[Int]()
  }

  property(".. relies on return value of predicate") = forAll(G.nonEmptyIntList) { ns =>
    dropWhile(ns, Function.const(false)) == ns
  }
}

class Week03PropertiesSpec extends Specification with ScalaCheck {

}

object Week03Spec extends Specification {
  "dropWhile" should {
    "do something" in {
      1 mustEqual 1
    }
  }
}