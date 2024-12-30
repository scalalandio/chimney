package io.scalaland.chimney.cats

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated}
import _root_.cats.syntax.validated.*
import io.scalaland.chimney.{partial, ChimneySpec}
import io.scalaland.chimney.partial.{Error, PathElement}
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.trip.*

class PartialResultToCatsDataConversionSpec extends ChimneySpec {

  group("conversion between Validated and partial.Result") {

    test("Valid should convert to partial.Result.Valid") {
      Validated.valid[partial.Result.Errors, User](User("John", 10, 140)).asResult ==>
        partial.Result.fromValue(User("John", 10, 140))
      Validated.validNec[partial.Error, User](User("John", 10, 140)).asResult ==>
        partial.Result.fromValue(User("John", 10, 140))
      Validated.validNec[String, User](User("John", 10, 140)).asResult ==>
        partial.Result.fromValue(User("John", 10, 140))
      Validated.validNel[partial.Error, User](User("John", 10, 140)).asResult ==>
        partial.Result.fromValue(User("John", 10, 140))
      Validated.validNel[String, User](User("John", 10, 140)).asResult ==>
        partial.Result.fromValue(User("John", 10, 140))
    }

    test("Invalid should convert to partial.Result.Errors") {
      Validated.invalid[partial.Result.Errors, User](partial.Result.Errors.fromString("error")).asResult ==>
        partial.Result.fromErrorString("error")
      Validated.invalidNec[partial.Error, User](partial.Error.fromString("error")).asResult ==>
        partial.Result.fromErrorString("error")
      Validated.invalidNec[String, User]("error").asResult ==>
        partial.Result.fromErrorString("error")
      Validated.invalidNel[partial.Error, User](partial.Error.fromString("error")).asResult ==>
        partial.Result.fromErrorString("error")
      Validated.invalidNel[String, User]("error").asResult ==>
        partial.Result.fromErrorString("error")
    }

    test("partial.Result.Valid should convert to Valid") {
      Person("John", 10, 140).transformIntoPartial[User].asValidated ==> Validated.valid(User("John", 10, 140))
      Person("John", 10, 140).transformIntoPartial[User].asValidatedNec ==> Validated.validNec(User("John", 10, 140))
      Person("John", 10, 140).transformIntoPartial[User].asValidatedNel ==> Validated.validNel(User("John", 10, 140))
      Person("John", 10, 140).transformIntoPartial[User].asValidatedChain ==> Validated.valid(User("John", 10, 140))
      Person("John", 10, 140).transformIntoPartial[User].asValidatedList ==> Validated.valid(User("John", 10, 140))

      Person("John", 10, 140).intoPartial[User].transform.asValidated ==> Validated.valid(User("John", 10, 140))
      Person("John", 10, 140).intoPartial[User].transform.asValidatedNec ==> Validated.validNec(User("John", 10, 140))
      Person("John", 10, 140).intoPartial[User].transform.asValidatedNel ==> Validated.validNel(User("John", 10, 140))
      Person("John", 10, 140).intoPartial[User].transform.asValidatedChain ==> Validated.valid(User("John", 10, 140))
      Person("John", 10, 140).intoPartial[User].transform.asValidatedList ==> Validated.valid(User("John", 10, 140))
    }

    group("partial.Result.Errors should convert to Invalid") {

      test("for String errors") {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(_.name, NonEmptyChain.of("foo").invalid.asResult)
          .withFieldConstPartial(_.age, Validated.valid(15).asResult)
          .withFieldConstPartial(_.height, NonEmptyList.of("abc", "def").invalid.asResult)
          .transform

        val expectedErr1 = Error.fromString("foo").prependErrorPath(PathElement.Provided("_.name", None))
        val expectedErr2 = Error.fromString("abc").prependErrorPath(PathElement.Provided("_.height", None))
        val expectedErr3 = Error.fromString("def").prependErrorPath(PathElement.Provided("_.height", None))

        result.asValidated ==> Validated.invalid(
          partial.Result.fromErrors(expectedErr1, expectedErr2, expectedErr3)
        )
        result.asValidatedNec ==> Validated.invalid(NonEmptyChain.of(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedNel ==> Validated.invalid(NonEmptyList.of(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedChain ==> Validated.invalid(Chain(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedList ==> Validated.invalid(List(expectedErr1, expectedErr2, expectedErr3))
      }

      test("for Throwable errors") {

        val ex1 = new RuntimeException("foo")
        val ex2 = new RuntimeException("abc")
        val ex3 = new RuntimeException("def")

        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(
            _.name,
            NonEmptyChain.of(Error.fromThrowable(ex1)).invalid.asResult
          )
          .withFieldConstPartial(_.age, Validated.valid(15).asResult)
          .withFieldConstPartial(
            _.height,
            NonEmptyList
              .of(Error.fromThrowable(ex2), Error.fromThrowable(ex3))
              .invalid
              .asResult
          )
          .transform

        val expectedErr1 = Error.fromThrowable(ex1).prependErrorPath(PathElement.Provided("_.name", None))
        val expectedErr2 = Error.fromThrowable(ex2).prependErrorPath(PathElement.Provided("_.height", None))
        val expectedErr3 = Error.fromThrowable(ex3).prependErrorPath(PathElement.Provided("_.height", None))

        result.asValidated ==> Validated.invalid(
          partial.Result.fromErrors(expectedErr1, expectedErr2, expectedErr3)
        )
        result.asValidatedNec ==> Validated.invalid(NonEmptyChain.of(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedNel ==> Validated.invalid(NonEmptyList.of(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedChain ==> Validated.invalid(Chain(expectedErr1, expectedErr2, expectedErr3))
        result.asValidatedList ==> Validated.invalid(List(expectedErr1, expectedErr2, expectedErr3))
      }
    }
  }
}
