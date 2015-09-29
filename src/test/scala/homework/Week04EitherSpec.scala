package homework 

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{ arbitrary }
import org.scalacheck.Gen.{ oneOf }
import org.scalacheck.Prop.{ forAll }
import org.scalacheck.Properties
import org.specs2.ScalaCheck
import org.specs2.mutable._

import homework.Either._

object EitherSpec extends Specification {
  // stuff
}

object EitherProps extends Properties("Either") {
  implicit def arbEither[T, U](implicit at: Arbitrary[T], au: Arbitrary[U]): Arbitrary[Either[T, U]] = 
    Arbitrary(oneOf(arbitrary[T].map(Left(_)), arbitrary[U].map(Right(_))))

  property(".. map over Either") = forAll { e: Either[Int, Int] =>
    val f = (x: Int) => x * 2

    e.map(f) == (e match {
      case Left(_)  => e
      case Right(v) => Right(f(v))
    })
  }

  property(".. flatMap over Either") = forAll { e: Either[Int, Int] =>
    val f = (x: Int) => x * 2
    val g = (x: Int) => Right(f(x))

    e.flatMap(g) == (e match {
      case Left(x) => Left(x)
      case Right(x) => Right(f(x))
    })
  }

  property(".. orElse over Either") = forAll { (e1: Either[Int, Int], e2: Either[Int, Int]) =>
    e1.orElse(e2) == (e1 match {
      case Right(_) => e1
      case Left(_) => e2
    })
  }

  property(".. map2 over Either") = forAll { (e1: Either[Int, Int], e2: Either[Int, Int]) =>
    val f = (x: Int, y: Int) => x + y

    e1.map2(e2)(f) == ((e1, e2) match {
      case (Left(_), _) => e1
      case (_, Left(_)) => e2
      case (Right(x), Right(y)) => Right(f(x, y))
    })
  }

  property(".. sequence over Either associativity") = forAll { (es: List[Either[String, Int]], es2: List[Either[String, Int]]) =>
    sequence(es ++ es2) == sequence(es).map2(sequence(es2))(_++_)
  }

  property(".. traverse over Either associativity") = forAll { (es: List[Either[String, Int]], es2: List[Either[String, Int]]) =>
    traverse(es ++ es2)(id) == traverse(es)(id).map2(traverse(es2)(id))(_++_)
  }
}
