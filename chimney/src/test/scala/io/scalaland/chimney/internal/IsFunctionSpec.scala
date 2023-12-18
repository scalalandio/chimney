package io.scalaland.chimney.internal
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.internal.runtime.IsFunction

class IsFunctionSpec extends ChimneySpec {

  test("IsFunction.Of checks out arities from 0 to 22") {

    def resolves[Fn, Out](implicit isFunction: IsFunction.Of[Fn, Out]): Unit = {
      val _ = isFunction
      ()
    }

    resolves[() => String, String]
    resolves[(Int) => String, String]
    resolves[(Int, Int) => String, String]
    resolves[(Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String, String]
    resolves[
      (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String,
      String
    ]
    resolves[
      (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String,
      String
    ]
    resolves[
      (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => String,
      String
    ]
    resolves[
      (
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
      ) => String,
      String
    ]
    resolves[
      (
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
      ) => String,
      String
    ]
  }
}
