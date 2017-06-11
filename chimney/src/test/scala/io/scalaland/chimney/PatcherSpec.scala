package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import io.scalaland.chimney.dsl._

class PatcherSpec extends WordSpec with MustMatchers {

  "Patcher DSL" should {

    "patch simple objects" in {

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(c: Double, a: Int)

      val foo = Foo(0, "", 0.0)
      val bar = Bar(10.0, 10)

      foo.patchWith(bar) mustBe
        Foo(10, "", 10.0)
    }
  }

}
