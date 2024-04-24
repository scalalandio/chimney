package io.scalaland.chimney.cats

import cats.data.NonEmptyLazyList
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.dsl.*

class CatsData213Spec extends ChimneySpec {

  test("DSL should handle transformation to and from cats.data.NonEmptyLazyList") {
    List("test").transformIntoPartial[NonEmptyLazyList[String]].asOption ==> Some(NonEmptyLazyList("test"))
    List.empty[String].transformIntoPartial[NonEmptyLazyList[String]].asOption ==> None

    NonEmptyLazyList("test").transformInto[List[String]] ==> List("test")
  }
}
