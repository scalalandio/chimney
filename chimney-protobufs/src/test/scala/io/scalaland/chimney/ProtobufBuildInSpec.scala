package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufBuildInSpec extends ChimneySpec {

  test("transform value classes between their primitive representations") {

    addressbook.PersonName("John").transformInto[String] ==> "John"
    addressbook.PersonId(5).transformInto[Int] ==> 5
    addressbook.Email("john@example.com").transformInto[String] ==> "john@example.com"
  }

  test("not compile if target type is wrong for value class") {

    compileErrorsFixed(""" addressbook.PersonName("John").transformInto[Int] """)
      .check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.addressbook.PersonName to scala.Int"
      )

    compileErrorsFixed(""" addressbook.PersonId(5).transformInto[String] """)
      .check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.addressbook.PersonId to java.lang.String"
      )

    compileErrorsFixed(""" addressbook.Email("john@example.com").transformInto[Float] """)
      .check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.addressbook.Email to scala.Float"
      )
  }
}
