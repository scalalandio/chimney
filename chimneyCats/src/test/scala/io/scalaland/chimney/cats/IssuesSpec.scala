package io.scalaland.chimney.cats

import cats.data.{Validated, ValidatedNec}
import io.scalaland.chimney.Transformer
import utest._

object IssuesSpec extends TestSuite {
  val tests: Tests = Tests {
    "fix issue #214" - {
      final case class BillingRowRaw(
                                      `Billing Zip/Postal Code`: String,
                                      `Billing Supplier Country (text only)`: String
                                    )

      final case class BillingRow(
                                   postalCode: String,
                                   billingCountry: String
                                 )

      val transformer = Transformer.defineF[ValidatedNec[String, +*], BillingRowRaw, BillingRow]
        .withFieldRenamed(_.`Billing Zip/Postal Code`, _.postalCode)
        .withFieldRenamed(_.`Billing Supplier Country (text only)`, _.billingCountry)
        .buildTransformer

      val expected = BillingRow("3152XX", "England")
      val result = transformer.transform(BillingRowRaw("3152XX", "England"))
      assert(result == Validated.validNec(expected))
    }
  }
}
