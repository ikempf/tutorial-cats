package example

import cats.kernel.Eq
import example.fixtures.GatoFixture
import org.scalatest.{MustMatchers, WordSpec}

class EqSpec extends WordSpec with MustMatchers with GatoFixture {

  import cats.syntax.eq._

  "Eq" must {

    "work on Ints" in {
      // TODO 01: find the right import
      import cats.instances.int._

      val eqInt = Eq[Int]
      eqInt.eqv(123, 123) must be(true)
      eqInt.eqv(123, 321) must be(false)

      (123 =!= 321) must be(true)
    }

    "work on Options" in {
      // TODO 01: find the right import
      import cats.syntax.option._
      import cats.instances.option._
      import cats.instances.int._
      // comparing options
      Eq[Option[Int]].eqv(Option(123), Option(123)) must be(true)

      Eq[Option[Int]].eqv(123.some, 123.some) must be(true)
    }

    "work on Gatos" in {
      // TODO 01: Equality should work between gatos
      import cats.syntax.option._

      Eq[Gato].eqv(cat1, cat2) must be(false)
      (cat1.some === none[Gato]) must be(false)

      Gato.isEqual(cat1, cat2) must be(false)

    }

  }

}
