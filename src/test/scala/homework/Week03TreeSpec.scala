package homework 

import org.specs2.mutable._
import org.specs2.ScalaCheck

import homework.Tree._
import homework.Tree.{ size => treeSize }

object Week03TreeSpec extends Specification {
  "size" should {
    val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
    treeSize(t1) mustEqual 3
  }

  "intMax" should {
    val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
    intMax(t1) mustEqual 10
  }

  "depth" should {
    "give the depth of the deepest node in the tree" in {
      val t1: Tree[Int] = Leaf(4)
      depth(t1) mustEqual(0)

      val t2: Tree[Int] = Branch(Leaf(5), Leaf(1))
      depth(t2) mustEqual(1)

      val t3: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      depth(t3) mustEqual(2)
    }
  }

  "fold" should {
    val t1: Tree[Int] = Leaf(4)
    val t2: Tree[String] = Branch(Leaf("01"), Branch(Branch(Leaf("02"), Leaf("03")), Leaf("04")))

    "perform a left-to-right, depth-first traversal of nodes" in {
      fold(t1)((v: Int) => v.toString)((left: String, right: String) => left + right) mustEqual "4"
      fold(t2)((v: String) => v)((left: String, right: String) => left + right) mustEqual "01020304"
    }

    "using constructors results in identity of tree" in {
      def mkLeaf(v: String): Tree[String] = Leaf(v)
      def mkBranch(left: Tree[String], right: Tree[String]): Tree[String] = Branch(left, right)

      fold(t2)(mkLeaf _)(mkBranch _) == t2
    }
  }

}
