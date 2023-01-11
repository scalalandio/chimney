package io.scalaland.chimney.cats

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated}
import _root_.cats.syntax.validated._
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.PartialTransformer.{Error, PathElement}
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.trip._
import utest._

object CatsValidatedPartialTransformerSpec extends TestSuite {

  val tests = Tests {

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

      val result = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, Validated.valid(15).toPartialTransformerResult)
        .withFieldConstPartial(_.height, NonEmptyList.of("abc", "def").invalid.toPartialTransformerResult)
        .transform

      val expectedErr1 = Error.ofString("abc").prependErrorPath(PathElement.Accessor("height"))
      val expectedErr2 = Error.ofString("def").prependErrorPath(PathElement.Accessor("height"))

      result.asValidated ==> Validated.invalid(PartialTransformer.Result.fromErrors(expectedErr1, expectedErr2))
      result.asValidatedNel ==> Validated.invalid(NonEmptyList.of(expectedErr1, expectedErr2))
      result.asValidatedNec ==> Validated.invalid(NonEmptyChain.of(expectedErr1, expectedErr2))
      result.asValidatedList ==> Validated.invalid(List(expectedErr1, expectedErr2))
      result.asValidatedChain ==> Validated.invalid(Chain(expectedErr1, expectedErr2))
    }
  }

}
