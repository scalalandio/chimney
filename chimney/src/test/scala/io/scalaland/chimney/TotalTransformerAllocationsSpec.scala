package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.nowarn

/** Verifies that [[dsl.TransformerInto]] (.into.withX.transform) chains with various combinations of data-carrying and
  * type-only modifiers produce correct transformations — both from fresh expressions and from `val` references.
  */
@nowarn("msg=unused import")
class TotalTransformerAllocationsSpec extends ChimneySpec {

  case class Source(a: Int, b: String, c: Double)
  case class Target(a: Int, b: String, c: Double)
  case class TargetRenamed(a: Int, bb: String, c: Double)
  case class TargetExtra(a: Int, b: String, c: Double, d: Long)
  case class TargetFewer(a: Int, c: Double)

  val src = Source(1, "hello", 3.14)

  group("fresh expression chains (.into)") {

    test("0 data-carrying modifiers") {
      src.into[Target].transform ==> Target(1, "hello", 3.14)
    }

    test("1 data-carrying modifier (withFieldConst)") {
      src.into[TargetExtra].withFieldConst(_.d, 42L).transform ==> TargetExtra(1, "hello", 3.14, 42L)
    }

    test("multiple data-carrying modifiers") {
      src
        .into[TargetExtra]
        .withFieldConst(_.d, 100L)
        .withFieldComputed(_.a, s => s.a + 10)
        .transform ==> TargetExtra(11, "hello", 3.14, 100L)
    }

    test("type-only modifier (withFieldRenamed)") {
      src.into[TargetRenamed].withFieldRenamed(_.b, _.bb).transform ==> TargetRenamed(1, "hello", 3.14)
    }

    test("data-carrying + type-only interleaved") {
      src
        .into[TargetExtra]
        .withFieldConst(_.d, 99L)
        .withFieldRenamed(_.b, _.b)
        .withFieldComputed(_.c, s => s.c * 2)
        .transform ==> TargetExtra(1, "hello", 6.28, 99L)
    }

    test("multiple type-only modifiers between data modifiers") {
      src
        .into[TargetExtra]
        .withFieldConst(_.d, 1L)
        .enableMethodAccessors
        .disableMethodAccessors
        .withFieldConst(_.a, 999)
        .transform ==> TargetExtra(999, "hello", 3.14, 1L)
    }

    test("only type-only modifiers") {
      src.into[Target].enableMethodAccessors.disableMethodAccessors.transform ==> Target(1, "hello", 3.14)
    }

    test("three consecutive data-carrying modifiers") {
      src
        .into[TargetExtra]
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "world")
        .withFieldConst(_.d, 7L)
        .transform ==> TargetExtra(10, "world", 3.14, 7L)
    }

    test("withFieldUnused (type-only) + withFieldConst (data)") {
      src
        .into[TargetFewer]
        .withFieldUnused(_.b)
        .withFieldConst(_.c, 0.0)
        .transform ==> TargetFewer(1, 0.0)
    }
  }

  group("val reference chains (.into then val)") {

    test("shared base with 0 data modifiers, then branching") {
      val base = src.into[TargetExtra]
      val r1 = base.withFieldConst(_.d, 10L).transform
      val r2 = base.withFieldConst(_.d, 20L).transform
      r1 ==> TargetExtra(1, "hello", 3.14, 10L)
      r2 ==> TargetExtra(1, "hello", 3.14, 20L)
    }

    test("shared base with 1 data modifier, then branching with more data") {
      val base = src.into[TargetExtra].withFieldConst(_.d, 0L)
      val r1 = base.withFieldComputed(_.a, _.a + 1).transform
      val r2 = base.withFieldComputed(_.a, _.a + 2).transform
      r1 ==> TargetExtra(2, "hello", 3.14, 0L)
      r2 ==> TargetExtra(3, "hello", 3.14, 0L)
    }

    test("shared base with data, then one branch adds data and other adds type-only") {
      val base = src.into[TargetExtra].withFieldConst(_.d, 5L)
      val r1 = base.withFieldComputed(_.a, _ => 100).transform
      val r2 = base.enableMethodAccessors.transform
      r1 ==> TargetExtra(100, "hello", 3.14, 5L)
      r2 ==> TargetExtra(1, "hello", 3.14, 5L)
    }

    test("deep chain from val: 3+ data modifiers after branching") {
      val base = src.into[TargetExtra].withFieldConst(_.d, 1L)
      val r = base
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "x")
        .withFieldComputed(_.c, _ => 0.0)
        .transform
      r ==> TargetExtra(10, "x", 0.0, 1L)
      // original base still works independently
      val r2 = base.transform
      r2 ==> TargetExtra(1, "hello", 3.14, 1L)
    }
  }
}
