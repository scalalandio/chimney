package io.scalaland.chimney.cats

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated}
import _root_.cats.syntax.validated._
import _root_.cats.syntax.semigroup._
import _root_.cats.syntax.semigroupal._
import cats.Semigroupal
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.{Error, PathElement}
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples.trip._
import utest._

object CatsValidatedPartialTransformerSpec extends TestSuite {

  val tests = Tests {

    "partial transformer errors semigroup instance" - {

      val e1 = partial.Result.Errors.fromString("test1")
      val e2 = partial.Result.Errors.fromString("test2")

      e1.combine(e2) ==> partial.Result.Errors.fromStrings("test1", "test2")
    }

    "partial transformer result semigroupal instance" - {

      "success" - {
        partial.Result
          .fromValue(1)
          .product(partial.Result.fromValue("abc")) ==>
          partial.Result.fromValue((1, "abc"))
      }

      "failure" - {
        Semigroupal[partial.Result].product(
          partial.Result.fromValue(1),
          partial.Result.fromErrorString("abc")
        ) ==>
          partial.Result.fromErrorString("abc")

        Semigroupal[partial.Result].product(
          partial.Result.fromErrorString("abc"),
          partial.Result.fromValue(1)
        ) ==>
          partial.Result.fromErrorString("abc")

        Semigroupal[partial.Result].product(
          partial.Result.fromErrorString("abc"),
          partial.Result.fromErrorString("def")
        ) ==>
          partial.Result.fromErrorStrings("abc", "def")
      }
    }

    "conversion between Validated and partial.Result" - {

      "transform always succeeds" - {

        Person("John", 10, 140).transformIntoPartial[User].asValidated ==> Validated.valid(User("John", 10, 140))
        Person("John", 10, 140).transformIntoPartial[User].asValidatedNel ==> Validated.validNel(User("John", 10, 140))
        Person("John", 10, 140).transformIntoPartial[User].asValidatedNec ==> Validated.validNec(User("John", 10, 140))
        Person("John", 10, 140).transformIntoPartial[User].asValidatedList ==> Validated.valid(User("John", 10, 140))
        Person("John", 10, 140).transformIntoPartial[User].asValidatedChain ==> Validated.valid(User("John", 10, 140))

        Person("John", 10, 140).intoPartial[User].transform.asValidated ==> Validated.valid(User("John", 10, 140))
        Person("John", 10, 140).intoPartial[User].transform.asValidatedNel ==> Validated.validNel(User("John", 10, 140))
        Person("John", 10, 140).intoPartial[User].transform.asValidatedNec ==> Validated.validNec(User("John", 10, 140))
        Person("John", 10, 140).intoPartial[User].transform.asValidatedList ==> Validated.valid(User("John", 10, 140))
        Person("John", 10, 140).intoPartial[User].transform.asValidatedChain ==> Validated.valid(User("John", 10, 140))
      }

      "transform always fails" - {

        "string errors" - {
          val result = Person("John", 10, 140)
            .intoPartial[User]
            .withFieldConstPartial(_.name, NonEmptyChain.of("foo").invalid.toPartialTransformerResult)
            .withFieldConstPartial(_.age, Validated.valid(15).toPartialTransformerResult)
            .withFieldConstPartial(_.height, NonEmptyList.of("abc", "def").invalid.toPartialTransformerResult)
            .transform

          val expectedErr1 = Error.ofString("foo").prependErrorPath(PathElement.Accessor("name"))
          val expectedErr2 = Error.ofString("abc").prependErrorPath(PathElement.Accessor("height"))
          val expectedErr3 = Error.ofString("def").prependErrorPath(PathElement.Accessor("height"))

          result.asValidated ==> Validated.invalid(
            partial.Result.fromErrors(expectedErr1, expectedErr2, expectedErr3)
          )
          result.asValidatedNel ==> Validated.invalid(NonEmptyList.of(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedNec ==> Validated.invalid(NonEmptyChain.of(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedList ==> Validated.invalid(List(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedChain ==> Validated.invalid(Chain(expectedErr1, expectedErr2, expectedErr3))
        }

        "throwable errors" - {

          val ex1 = new RuntimeException("foo")
          val ex2 = new RuntimeException("abc")
          val ex3 = new RuntimeException("def")

          val result = Person("John", 10, 140)
            .intoPartial[User]
            .withFieldConstPartial(
              _.name,
              NonEmptyChain.of(Error.ofThrowable(ex1)).invalid.toPartialTransformerResult
            )
            .withFieldConstPartial(_.age, Validated.valid(15).toPartialTransformerResult)
            .withFieldConstPartial(
              _.height,
              NonEmptyList
                .of(Error.ofThrowable(ex2), Error.ofThrowable(ex3))
                .invalid
                .toPartialTransformerResult
            )
            .transform

          val expectedErr1 = Error.ofThrowable(ex1).prependErrorPath(PathElement.Accessor("name"))
          val expectedErr2 = Error.ofThrowable(ex2).prependErrorPath(PathElement.Accessor("height"))
          val expectedErr3 = Error.ofThrowable(ex3).prependErrorPath(PathElement.Accessor("height"))

          result.asValidated ==> Validated.invalid(
            partial.Result.fromErrors(expectedErr1, expectedErr2, expectedErr3)
          )
          result.asValidatedNel ==> Validated.invalid(NonEmptyList.of(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedNec ==> Validated.invalid(NonEmptyChain.of(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedList ==> Validated.invalid(List(expectedErr1, expectedErr2, expectedErr3))
          result.asValidatedChain ==> Validated.invalid(Chain(expectedErr1, expectedErr2, expectedErr3))
        }
      }
    }
  }

}
