package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.scala213 as s213
import io.scalaland.chimney.scala3 as s3
import scala.util.chaining.*

class MacroCrossCompilationScala3Spec extends ChimneySpec {

  group("Scala 2.13 compiler analyzing Scala 3 code and Scala 3 compiler analyzing Scala 2.13 code") {

    test("should handle reading from and writing to sealed trait/enum") {
      // only Scala 3 makes distinction between vals in mattern matching :/
      (s3.Enums.Foo.A: s3.Enums.Foo).transformInto[s213.Sealed.Foo] ==> s213.Sealed.Foo.A
      (s3.Enums.Foo.B: s3.Enums.Foo).transformInto[s213.Sealed.Foo] ==> s213.Sealed.Foo.B
    }
  }
}
