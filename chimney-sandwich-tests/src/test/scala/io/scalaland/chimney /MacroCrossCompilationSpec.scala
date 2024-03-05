package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.scala213 as s213
import io.scalaland.chimney.scala3 as s3

class MacroCrossCompilationSpec extends ChimneySpec {

  group("Scala 2.13 compiler analyzing Scala 3 code and Scala 3 compiler analyzing Scala 2.13 code") {

    test("should handle reading from and writing to monomorphic classes") {
      s213.Monomorphic.Foo(1, "2", 3.0, true).transformInto[s3.Monomorphic.Bar]
      s3.Monomorphic.Foo(1, "2", 3.0, true).transformInto[s213.Monomorphic.Bar]
      implicit val bool2int: Transformer[Boolean, Int] = b => if (b) 1 else 0
      s213.Monomorphic.Foo(1, "2", 3.0, true).into[s3.Monomorphic.Baz].transform
      s3.Monomorphic.Foo(1, "2", 3.0, true).into[s213.Monomorphic.Baz].transform
    }
  }
}
