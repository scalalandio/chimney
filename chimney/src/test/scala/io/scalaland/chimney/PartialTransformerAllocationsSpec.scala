package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.nowarn

/** Verifies that [[dsl.PartialTransformerInto]] (.intoPartial.withX.transform) chains with various combinations of
  * data-carrying and type-only modifiers produce correct transformations — both from fresh expressions and from `val`
  * references.
  */
@nowarn("msg=unused import")
class PartialTransformerAllocationsSpec extends ChimneySpec {

  case class Source(a: Int, b: String, c: Double)
  case class Target(a: Int, b: String, c: Double)
  case class TargetExtra(a: Int, b: String, c: Double, d: Long)
  case class TargetFewer(a: Int, c: Double)

  val src = Source(1, "hello", 3.14)

  group("fresh expression chains (.intoPartial)") {

    test("0 data-carrying modifiers") {
      src.intoPartial[Target].transform.asOption ==> Some(Target(1, "hello", 3.14))
    }

    test("1 data-carrying modifier (withFieldConst)") {
      src.intoPartial[TargetExtra].withFieldConst(_.d, 42L).transform.asOption ==>
        Some(TargetExtra(1, "hello", 3.14, 42L))
    }

    test("1 data-carrying modifier (withFieldConstPartial)") {
      src
        .intoPartial[TargetExtra]
        .withFieldConstPartial(_.d, partial.Result.fromValue(42L))
        .transform
        .asOption ==>
        Some(TargetExtra(1, "hello", 3.14, 42L))
    }

    test("multiple data-carrying modifiers") {
      src
        .intoPartial[TargetExtra]
        .withFieldConst(_.d, 100L)
        .withFieldComputed(_.a, s => s.a + 10)
        .transform
        .asOption ==> Some(TargetExtra(11, "hello", 3.14, 100L))
    }

    test("data-carrying + type-only interleaved") {
      src
        .intoPartial[TargetExtra]
        .withFieldConst(_.d, 99L)
        .enableMethodAccessors
        .withFieldComputed(_.c, s => s.c * 2)
        .transform
        .asOption ==> Some(TargetExtra(1, "hello", 6.28, 99L))
    }

    test("multiple type-only modifiers between data modifiers") {
      src
        .intoPartial[TargetExtra]
        .withFieldConst(_.d, 1L)
        .enableMethodAccessors
        .disableMethodAccessors
        .withFieldConst(_.a, 999)
        .transform
        .asOption ==> Some(TargetExtra(999, "hello", 3.14, 1L))
    }

    test("three consecutive data-carrying modifiers") {
      src
        .intoPartial[TargetExtra]
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "world")
        .withFieldConst(_.d, 7L)
        .transform
        .asOption ==> Some(TargetExtra(10, "world", 3.14, 7L))
    }

    test("withFieldComputedPartial") {
      src
        .intoPartial[TargetExtra]
        .withFieldComputedPartial(_.d, s => partial.Result.fromValue(s.a.toLong))
        .transform
        .asOption ==> Some(TargetExtra(1, "hello", 3.14, 1L))
    }

    test("withFieldUnused + withFieldConst") {
      src
        .intoPartial[TargetFewer]
        .withFieldUnused(_.b)
        .withFieldConst(_.c, 0.0)
        .transform
        .asOption ==> Some(TargetFewer(1, 0.0))
    }
  }

  group("val reference chains (.intoPartial then val)") {

    test("shared base with 0 data modifiers, then branching") {
      val base = src.intoPartial[TargetExtra]
      val r1 = base.withFieldConst(_.d, 10L).transform.asOption
      val r2 = base.withFieldConst(_.d, 20L).transform.asOption
      r1 ==> Some(TargetExtra(1, "hello", 3.14, 10L))
      r2 ==> Some(TargetExtra(1, "hello", 3.14, 20L))
    }

    test("shared base with data, then branching") {
      val base = src.intoPartial[TargetExtra].withFieldConst(_.d, 0L)
      val r1 = base.withFieldComputed(_.a, _.a + 1).transform.asOption
      val r2 = base.withFieldComputed(_.a, _.a + 2).transform.asOption
      r1 ==> Some(TargetExtra(2, "hello", 3.14, 0L))
      r2 ==> Some(TargetExtra(3, "hello", 3.14, 0L))
    }

    test("deep chain from val after branching") {
      val base = src.intoPartial[TargetExtra].withFieldConst(_.d, 1L)
      val r = base
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "x")
        .transform
        .asOption
      r ==> Some(TargetExtra(10, "x", 3.14, 1L))
      val r2 = base.transform.asOption
      r2 ==> Some(TargetExtra(1, "hello", 3.14, 1L))
    }
  }
}
