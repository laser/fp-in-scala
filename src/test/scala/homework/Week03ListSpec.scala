package homework 

import org.scalacheck.Gen.{ listOfN, posNum }
import org.scalacheck.Prop.{ forAll }
import org.scalacheck.Properties
import org.specs2.mutable._
import org.specs2.ScalaCheck

import homework.ListUtil._

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
  property(".. using an arbitrary predicate") = forAll { (ns: List[Int], p: Int => Boolean) =>
    dropWhile(ns, p).length <= ns.length
  }

  property(".. relies on return value of predicate") = forAll(G.nonEmptyIntList) { ns =>
    dropWhile(ns, Function.const(false)) == ns
    dropWhile(ns, Function.const(true)) == List[Int]()
  }
}

object ListInitProp extends Properties("init") {
  property("..") = forAll(G.nonEmptyIntList) { ns =>
    init(ns).length == ns.length-1
  }
}

object ListLengthProp extends Properties("length") {
  property(".. behaves identically to the provided length method") = forAll(G.nonEmptyIntList) { ns =>
    length(ns) == ns.length
  }
}

object ListReverseProp extends Properties("reverse") {
  property(".. behaves identically to the provided reverse method") = forAll { ns: List[Int] =>
    reverse(ns) == ns.reverse
  }
}

object ListAppendProp extends Properties("append") {
  property(".. behaves identically to ++ over list") = forAll { (ss1: List[String], ss2: List[String]) =>
    append(ss1, ss2) == ss1 ++ ss2
  }
}

object ListFlattenProp extends Properties("flatten") {
  property(".. behaves identically to flatten over list") = forAll { lols: List[List[String]] =>
    flatten(lols) == lols.flatten
  }
}

object ListIncrementByOneProp extends Properties("increment by one") {
  property("..") = forAll { ns: List[Int] => 
    incrementEachByOne(ns).length == ns.length
    sum(incrementEachByOne(ns)) == (sum(ns) + ns.length)
  }
}

object ListFilterViaFlatMapProp extends Properties("filterViaFlatMap") {
  val predicate = (i: Int) => i > 0

  property(".. behaves identically to filter member") = forAll { is: List[Int] =>
    is.filter(predicate) == filterViaFlatMap(is)(predicate)
  }

  property(".. behaves identically to our filter method") = forAll { is: List[Int] =>
    filter(is)(predicate) == filterViaFlatMap(is)(predicate)
  }
}

object Week03ListSpec extends Specification {
  "init" should {
    "preserve the original ordering" in {
      init(List[Int](1,2,3,4)) mustEqual List[Int](1,2,3)
    }
  }

  "flatMap" should {
    "work?" in {
      flatMap(List[Int](1,2,3)) { item =>
        List[Int](item, item, item)
      } mustEqual List[Int](1,1,1,2,2,2,3,3,3)
    }
  }
}
