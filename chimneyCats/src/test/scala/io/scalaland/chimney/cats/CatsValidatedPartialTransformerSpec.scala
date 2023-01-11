package io.scalaland.chimney.cats

import _root_.cats.data.{NonEmptyList, Validated}
import _root_.cats.syntax.validated._
import io.scalaland.chimney.PartialTransformer.{Error, PathElement}
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.trip._
import utest._

object CatsValidatedPartialTransformerSpec extends TestSuite {

  val tests = Tests {

    "transform always succeeds" - {

      Person("John", 10, 140).transformIntoPartial[User].asValidated ==> Validated.valid(User("John", 10, 140))

      Person("John", 10, 140).intoPartial[User].transform.asValidated ==> Validated.valid(User("John", 10, 140))
    }

    "transform always fails" - {

      Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, Validated.valid(15).toPartialTransformerResult)
        .withFieldConstPartial(_.height, NonEmptyList.of("abc", "def").invalid.toPartialTransformerResult)
        .transform
        .asValidatedNel ==> Validated.invalid(
        NonEmptyList.of(
          Error.ofString("abc").prependErrorPath(PathElement.Accessor("height")),
          Error.ofString("def").prependErrorPath(PathElement.Accessor("height"))
        )
      )
    }
  }

}
