package homework 

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{ listOfN, posNum, oneOf }
import org.scalacheck.Prop.{ forAll }
import org.scalacheck.Properties
import org.specs2.mutable._
import org.specs2.ScalaCheck

import homework.Week03._

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

object Week03Spec extends Specification {
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

  "treeDepth" should {
    "give the depth of the deepest node in the tree" in {
      val t1: Tree[Int] = Leaf(4)
      treeDepth(t1) mustEqual(0)

      val t2: Tree[Int] = Branch(Leaf(5), Leaf(1))
      treeDepth(t2) mustEqual(1)

      val t3: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      treeDepth(t3) mustEqual(2)
    }
  }

  "treeFold" should {
    "perform a left-to-right, depth-first traversal of nodes" in {
      val t1: Tree[Int] = Leaf(4)
      treeFold((v: Int) => v.toString)((left: String, right: String) => left + right)(t1) mustEqual "4"

      val t2: Tree[String] = Branch(Leaf("01"), Branch(Branch(Leaf("02"), Leaf("03")), Leaf("04")))
      treeFold((v: String) => v)((left: String, right: String) => left + right)(t2) mustEqual "01020304"
    }

    "using constructors results in identity of tree" in {
      def mkLeaf(v: String): Tree[String] = Leaf(v)
      def mkBranch(left: Tree[String], right: Tree[String]): Tree[String] = Branch(left, right)

      val t: Tree[String] = Branch(Leaf("01"), Branch(Branch(Leaf("02"), Leaf("03")), Leaf("04")))
      treeFold(mkLeaf _)(mkBranch _)(t) == t
    }
  }
}
