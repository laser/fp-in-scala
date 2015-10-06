package homework

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters

trait HomeworkPropertyCheck extends Specification with ScalaCheck {
  implicit val ps = Parameters(minTestsOk=100, workers=2)
}
