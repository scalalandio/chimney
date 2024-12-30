package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.lenslike.*

class PartialTransformerLensLikeSpec extends ChimneySpec {

  test("update case class field") {
    Foo(10, "example").intoPartial[Foo[Int]].withFieldConst(_.value, 20).transform.asOption ==> Some(Foo(20, "example"))
    Foo(Foo(10, "example"), "example")
      .intoPartial[Foo[Foo[Int]]]
      .withFieldConst(_.value.value, 20)
      .transform
      .asOption ==> Some(Foo(Foo(20, "example"), "example"))
  }

  test("update sealed hierarchy") {
    Foo[Bar[Int]](Bar.Baz(10), "example")
      .intoPartial[Foo[Bar[Int]]]
      .withFieldConst(_.value.matching[Bar.Baz[Int]].value, 20)
      .transform
      .asOption ==> Some(Foo[Bar[Int]](Bar.Baz(20), "example"))
    (Bar.Baz(Foo(10, "example")): Bar[Foo[Int]])
      .intoPartial[Bar[Foo[Int]]]
      .withFieldConst(_.matching[Bar.Baz[Foo[Int]]].value.value, 20)
      .transform
      .asOption ==> Some(Bar.Baz(Foo(20, "example")))
  }

  test("update case class with Option") {
    WithOption(Some(Foo(10, "example")))
      .intoPartial[WithOption[Foo[Int]]]
      .withFieldConst(_.option.matchingSome.value, 20)
      .transform
      .asOption ==> Some(WithOption(Some(Foo(20, "example"))))
    WithOption(Some(Foo(10, "example")))
      .intoPartial[WithOption[Foo[Int]]]
      .withFieldConst(_.option.matching[Some[Foo[Int]]].value.value, 20)
      .transform
      .asOption ==> Some(WithOption(Some(Foo(20, "example"))))
    Foo(WithOption(Some(10)), "example")
      .intoPartial[Foo[WithOption[Int]]]
      .withFieldConst(_.value.option.matchingSome, 20)
      .transform
      .asOption ==> Some(Foo(WithOption(Some(20)), "example"))
    Foo(WithOption(Some(10)), "example")
      .intoPartial[Foo[WithOption[Int]]]
      .withFieldConst(_.value.option.matching[Some[Int]].value, 20)
      .transform
      .asOption ==> Some(Foo(WithOption(Some(20)), "example"))
  }

  test("update case class with Either") {
    WithEither[Foo[Int], Foo[Int]](Left(Foo(10, "example")))
      .intoPartial[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matchingLeft.value, 20)
      .withFieldConst(_.either.matchingRight.value, 30)
      .transform
      .asOption ==> Some(WithEither(Left(Foo(20, "example"))))
    WithEither[Foo[Int], Foo[Int]](Left(Foo(10, "example")))
      .intoPartial[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matching[Left[Foo[Int], Foo[Int]]].value.value, 20)
      .withFieldConst(_.either.matching[Right[Foo[Int], Foo[Int]]].value.value, 30)
      .transform
      .asOption ==> Some(WithEither(Left(Foo(20, "example"))))
    WithEither[Foo[Int], Foo[Int]](Right(Foo(10, "example")))
      .intoPartial[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matchingLeft.value, 20)
      .withFieldConst(_.either.matchingRight.value, 30)
      .transform
      .asOption ==> Some(WithEither(Right(Foo(30, "example"))))
    WithEither[Foo[Int], Foo[Int]](Right(Foo(10, "example")))
      .intoPartial[WithEither[Foo[Int], Foo[Int]]]
      .withFieldConst(_.either.matching[Left[Foo[Int], Foo[Int]]].value.value, 20)
      .withFieldConst(_.either.matching[Right[Foo[Int], Foo[Int]]].value.value, 30)
      .transform
      .asOption ==> Some(WithEither(Right(Foo(30, "example"))))
    Foo[WithEither[Int, Int]](WithEither(Left(10)), "example")
      .intoPartial[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matchingLeft, 20)
      .withFieldConst(_.value.either.matchingRight, 30)
      .transform
      .asOption ==> Some(Foo(WithEither(Left(20)), "example"))
    Foo[WithEither[Int, Int]](WithEither(Left(10)), "example")
      .intoPartial[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matching[Left[Int, Int]].value, 20)
      .withFieldConst(_.value.either.matching[Right[Int, Int]].value, 30)
      .transform
      .asOption ==> Some(Foo(WithEither(Left(20)), "example"))
    Foo[WithEither[Int, Int]](WithEither(Right(10)), "example")
      .intoPartial[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matchingLeft, 20)
      .withFieldConst(_.value.either.matchingRight, 30)
      .transform
      .asOption ==> Some(Foo(WithEither(Right(30)), "example"))
    Foo[WithEither[Int, Int]](WithEither(Right(10)), "example")
      .intoPartial[Foo[WithEither[Int, Int]]]
      .withFieldConst(_.value.either.matching[Left[Int, Int]].value, 20)
      .withFieldConst(_.value.either.matching[Right[Int, Int]].value, 30)
      .transform
      .asOption ==> Some(Foo(WithEither(Right(30)), "example"))
  }

  test("update case class with collection") {
    WithList(List(Foo(10, "example")))
      .intoPartial[WithList[Foo[Int]]]
      .withFieldConst(_.list.everyItem.value, 20)
      .transform
      .asOption ==> Some(WithList(List(Foo(20, "example"))))
    Foo(WithList(List(10)), "example")
      .intoPartial[Foo[WithList[Int]]]
      .withFieldConst(_.value.list.everyItem, 20)
      .transform
      .asOption ==> Some(Foo(WithList(List(20)), "example"))
  }

  test("update case class with Map") {
    WithMap[Foo[Int], Foo[Int]](Map(Foo(10, "example") -> Foo(20, "example2")))
      .intoPartial[WithMap[Foo[Int], Foo[Int]]]
      .withFieldConst(_.map.everyMapKey.value, 30)
      .withFieldConst(_.map.everyMapValue.value, 40)
      .transform
      .asOption ==> Some(WithMap[Foo[Int], Foo[Int]](Map(Foo(30, "example") -> Foo(40, "example2"))))
    Foo[WithMap[Int, Int]](WithMap(Map(10 -> 20)), "example")
      .intoPartial[Foo[WithMap[Int, Int]]]
      .withFieldConst(_.value.map.everyMapKey, 30)
      .withFieldConst(_.value.map.everyMapValue, 40)
      .transform
      .asOption ==> Some(Foo[WithMap[Int, Int]](WithMap(Map(30 -> 40)), "example"))
  }

  test("update deep complex nesting") {
    Foo(
      WithOption(Some(WithList(List(WithMap(Map(10 -> WithEither[Int, Foo[Int]](Right(Foo(10, "example2"))))))))),
      "example"
    )
      .intoPartial[Foo[WithOption[WithList[WithMap[Int, WithEither[Int, Foo[Int]]]]]]]
      .withFieldConst(_.value.option.matchingSome.list.everyItem.map.everyMapValue.either.matchingRight.value, 20)
      .transform
      .asOption ==> Some(
      Foo(
        WithOption(Some(WithList(List(WithMap(Map(10 -> WithEither[Int, Foo[Int]](Right(Foo(20, "example2"))))))))),
        "example"
      )
    )
  }
}
