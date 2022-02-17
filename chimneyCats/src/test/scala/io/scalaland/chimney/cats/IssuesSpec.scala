package io.scalaland.chimney.cats

import cats.data.{Validated, ValidatedNec}
import io.scalaland.chimney.Transformer
import utest._

object IssuesSpec extends TestSuite {
  val tests: Tests = Tests {
    "fix issue #214" - {
      final case class Foo(
                                      `Billing Zip/Postal Code`: String,
                                      `Shipping Zip/Postal Code`: String,
                                      `Billing Supplier Country (text only)`: String
                                    )

      final case class Bar(
                            `Billing Zip/Postal Code`: String,
                            `Shipping Zip/Postal Code`: String,
                            `Billing Supplier Country (text only)`: String
                                 )

      val transformer = Transformer
        .defineF[ValidatedNec[String, +*], Foo, Bar]
        .buildTransformer

      val expected = Bar("3152XX", "3152XX", "England")
      val result = transformer.transform(Foo("3152XX", "3152XX", "England"))
      assert(result == Validated.validNec(expected))
    }
  }
}
