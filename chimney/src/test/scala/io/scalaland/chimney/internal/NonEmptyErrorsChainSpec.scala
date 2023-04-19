package io.scalaland.chimney.internal

import io.scalaland.chimney.partial
import utest.*

object NonEmptyErrorsChainSpec extends TestSuite {

  val tests = Tests {

    def err(i: Int): partial.Error = partial.Error.fromString(s"err$i")

    test("NonEmptyErrorsChainSpec.isEmpty") {
      NonEmptyErrorsChain
        .from(err(0))
        .isEmpty ==> false
    }

    test("NonEmptyErrorsChainSpec.prependPath") {

      test("basic") {
        val pathElement = partial.PathElement.Accessor("field1")

        val errorsEmptyPath = NonEmptyErrorsChain.from(err(1), err(2), err(3))

        val errorsNonEmptyPath = NonEmptyErrorsChain
          .from(
            err(1).prependErrorPath(pathElement),
            err(2).prependErrorPath(pathElement),
            err(3).prependErrorPath(pathElement)
          )

        errorsEmptyPath.prependPath(pathElement) ==> errorsNonEmptyPath
      }

      test("multiply nested") {

        val pathElement1 = partial.PathElement.Accessor("field1")
        val pathElement2 = partial.PathElement.Index(4)

        val errors1 = NonEmptyErrorsChain.from(err(1), err(2), err(3))
        val errors2 = NonEmptyErrorsChain.from(err(4), err(5), err(6))

        (errors1.prependPath(pathElement1) ++ errors2).prependPath(pathElement2) ==> NonEmptyErrorsChain
          .from(
            err(1).prependErrorPath(pathElement1).prependErrorPath(pathElement2),
            err(2).prependErrorPath(pathElement1).prependErrorPath(pathElement2),
            err(3).prependErrorPath(pathElement1).prependErrorPath(pathElement2),
            err(4).prependErrorPath(pathElement2),
            err(5).prependErrorPath(pathElement2),
            err(6).prependErrorPath(pathElement2)
          )
      }
    }

    test("NonEmptyErrorsChainSpec.iterator") {

      val errors = NonEmptyErrorsChain
        .from(err(1), err(2), err(3))

      errors.iterator.toList ==> List(err(1), err(2), err(3))
    }

    test("NonEmptyErrorsChainSpec.++") {

      val errors1 = NonEmptyErrorsChain.from(err(1), err(2), err(3))
      val errors2 = NonEmptyErrorsChain.from(err(4), err(5))
      val errors3 = NonEmptyErrorsChain.from(err(6))

      test("basic") {
        (errors1 ++ errors2) ==> NonEmptyErrorsChain.from(err(1), err(2), err(3), err(4), err(5))
      }

      test("associativity") {
        ((errors1 ++ errors2) ++ errors3) ==> (errors1 ++ (errors2 ++ errors3))
      }
    }

    test("NonEmptyErrorsChainSpec.equals/hashCode") {

      val errors1 = NonEmptyErrorsChain.from(err(1), err(2)) ++ NonEmptyErrorsChain.from(err(3))
      val errors2 = NonEmptyErrorsChain.from(err(1)) ++ NonEmptyErrorsChain.from(err(2), err(3))
      val errors3 = NonEmptyErrorsChain.from(err(1)) ++ NonEmptyErrorsChain.from(err(2))

      errors1.equals(errors2) ==> true
      errors2.equals(errors1) ==> true
      errors1.hashCode() ==> errors2.hashCode()

      errors1.equals(errors3) ==> false
      errors3.equals(errors1) ==> false
      // hash codes may differ

      errors2.equals(errors3) ==> false
      errors3.equals(errors2) ==> false
      // hash codes may differ

      // keep symmetry
      errors1.equals(List(err(1), err(2), err(3))) ==> false
      List(err(1), err(2), err(3)).equals(errors1) ==> false
    }
  }
}
