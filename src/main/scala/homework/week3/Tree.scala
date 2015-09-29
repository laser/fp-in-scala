package homework

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25, page 46
  def size[A](t: Tree[A]): Int = fold(t)(Function.const(1))(_+_)

  // Exercise 3.26, page 46
  def intMax(t: Tree[Int]): Int = 
    fold(t)((x: Int) => x)((left: Int, right: Int) => if (left > right) left else right)

  // Exercise 3.27, page 46
  def depth[A](t: Tree[A]): Int = 
    fold(t)(Function.const(0))((left: Int, right: Int) => if (left > right) left+1 else right+1)

  // Exercise 3.28, page 46
  // note: the compiler could not figure out that Leaf(f(v)) was of type Tree[B]
  //       so I had to give it a hint
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    fold(t)((v: A) => Leaf(f(v)): Tree[B])(Branch(_, _))

  // Exercise 3.29, page 47
  def fold[A, B](t: Tree[A])(leaf: A => B)(branch: (B, B) => B): B = t match {
    case Leaf(value) => leaf(value)
    case Branch(left, right) => branch(fold(left)(leaf)(branch), fold(right)(leaf)(branch))
  }
}
