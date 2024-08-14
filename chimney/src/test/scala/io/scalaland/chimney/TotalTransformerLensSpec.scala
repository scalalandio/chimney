package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.lenslike.*

class TotalTransformerLensSpec extends ChimneySpec {

  test("update case class field") {
    Foo(10, "example").into[Foo[Int]].withFieldConst(_.value, 20).transform ==> Foo(20, "example")
    Foo(Foo(10, "example"), "example")
      .into[Foo[Foo[Int]]]
      .withFieldConst(_.value.value, 20)
      .transform ==> Foo(Foo(20, "example"), "example")
  }

  // TODO: matching[Subtype]

  test("update case class with Option") {
    WithOption(Some(Foo(10, "example")))
      .into[WithOption[Foo[Int]]]
      .withFieldConst(_.option.matchingSome.value, 20)
      .transform ==> WithOption(Some(Foo(20, "example")))
    Foo(WithOption(Some(10)), "example")
      .into[Foo[WithOption[Int]]]
      .withFieldConst(_.value.option.matchingSome, 20)
      .transform ==> Foo(WithOption(Some(20)), "example")
  }

  test("update case class with Either") {
    WithEither[Foo[Int], Foo[Int]](Left(Foo(10, "example")))
      .into[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matchingLeft.value, 20)
      .withFieldConst(_.either.matchingRight.value, 30)
      .transform ==> WithEither(Left(Foo(20, "example")))
    WithEither[Foo[Int], Foo[Int]](Right(Foo(10, "example")))
      .into[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matchingLeft.value, 20)
      .withFieldConst(_.either.matchingRight.value, 30)
      .transform ==> WithEither(Right(Foo(30, "example")))
    Foo[WithEither[Int, Int]](WithEither(Left(10)), "example")
      .into[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matchingLeft, 20)
      .withFieldConst(_.value.either.matchingRight, 30)
      .transform ==> Foo(WithEither(Left(20)), "example")
    Foo[WithEither[Int, Int]](WithEither(Right(10)), "example")
      .into[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matchingLeft, 20)
      .withFieldConst(_.value.either.matchingRight, 30)
      .transform ==> Foo(WithEither(Right(30)), "example")
  }

  test("update case class with collection") {
    WithList(List(Foo(10, "example")))
      .into[WithList[Foo[Int]]]
      .withFieldConst(_.list.everyItem.value, 20)
      .transform ==> WithList(List(Foo(20, "example")))
    Foo(WithList(List(10)), "example")
      .into[Foo[WithList[Int]]]
      .withFieldConst(_.value.list.everyItem, 20)
      .transform ==> Foo(WithList(List(20)), "example")
  }

  test("update case class with Map") {
    WithMap[Foo[Int], Foo[Int]](Map(Foo(10, "example") -> Foo(20, "example2")))
      .into[WithMap[Foo[Int], Foo[Int]]]
      .withFieldConst(_.map.everyMapKey.value, 30)
      .withFieldConst(_.map.everyMapValue.value, 40)
      .transform ==> WithMap[Foo[Int], Foo[Int]](Map(Foo(30, "example") -> Foo(40, "example2")))
    Foo[WithMap[Int, Int]](WithMap(Map(10 -> 20)), "example")
      .into[Foo[WithMap[Int, Int]]]
      .withFieldConst(_.value.map.everyMapKey, 30)
      .withFieldConst(_.value.map.everyMapValue, 40)
      .transform ==> Foo[WithMap[Int, Int]](WithMap(Map(30 -> 40)), "example")
  }

  test("update deep complex nesting") {
    Foo(
      WithOption(Some(WithList(List(WithMap(Map(10 -> WithEither[Int, Foo[Int]](Right(Foo(10, "example2"))))))))),
      "example"
    )
      .into[Foo[WithOption[WithList[WithMap[Int, WithEither[Int, Foo[Int]]]]]]]
      .withFieldConst(_.value.option.matchingSome.list.everyItem.map.everyMapValue.either.matchingRight.value, 20)
      .transform ==> Foo(
      WithOption(Some(WithList(List(WithMap(Map(10 -> WithEither[Int, Foo[Int]](Right(Foo(20, "example2"))))))))),
      "example"
    )
  }
}
