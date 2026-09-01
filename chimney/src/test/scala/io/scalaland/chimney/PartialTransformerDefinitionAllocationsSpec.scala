package io.scalaland.chimney

/** Verifies that [[dsl.PartialTransformerDefinition]] chains with various combinations of data-carrying and type-only
  * modifiers produce correct transformations — both from fresh expressions and from `val` references (branching).
  */
class PartialTransformerDefinitionAllocationsSpec extends ChimneySpec {

  case class Source(a: Int, b: String, c: Double)
  case class Target(a: Int, b: String, c: Double)
  case class TargetExtra(a: Int, b: String, c: Double, d: Long)
  case class TargetFewer(a: Int, c: Double)

  val src = Source(1, "hello", 3.14)

  group("fresh expression chains") {

    test("0 data-carrying modifiers (empty chain)") {
      val t = PartialTransformer.define[Source, Target].buildTransformer
      t.transform(src).asOption ==> Some(Target(1, "hello", 3.14))
    }

    test("1 data-carrying modifier (withFieldConst)") {
      val t = PartialTransformer.define[Source, TargetExtra].withFieldConst(_.d, 42L).buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 42L))
    }

    test("1 data-carrying modifier (withFieldConstPartial)") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldConstPartial(_.d, partial.Result.fromValue(42L))
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 42L))
    }

    test("multiple data-carrying modifiers") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 100L)
        .withFieldComputed(_.a, s => s.a + 10)
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(11, "hello", 3.14, 100L))
    }

    test("data-carrying + type-only interleaved") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 99L)
        .enableMethodAccessors
        .withFieldComputed(_.c, s => s.c * 2)
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(1, "hello", 6.28, 99L))
    }

    test("multiple type-only modifiers between data modifiers") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 1L)
        .enableMethodAccessors
        .disableMethodAccessors
        .withFieldConst(_.a, 999)
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(999, "hello", 3.14, 1L))
    }

    test("three consecutive data-carrying modifiers") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "world")
        .withFieldConst(_.d, 7L)
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(10, "world", 3.14, 7L))
    }

    test("withFieldComputedPartial") {
      val t = PartialTransformer
        .define[Source, TargetExtra]
        .withFieldComputedPartial(_.d, s => partial.Result.fromValue(s.a.toLong))
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 1L))
    }

    test("withFieldUnused + withFieldConst") {
      val t = PartialTransformer
        .define[Source, TargetFewer]
        .withFieldUnused(_.b)
        .withFieldConst(_.c, 0.0)
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetFewer(1, 0.0))
    }
  }

  group("val reference chains (branching)") {

    test("shared base with 0 data modifiers, then branching") {
      val base = PartialTransformer.define[Source, TargetExtra]
      val t1 = base.withFieldConst(_.d, 10L).buildTransformer
      val t2 = base.withFieldConst(_.d, 20L).buildTransformer
      t1.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 10L))
      t2.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 20L))
    }

    test("shared base with data, then branching with more data") {
      val base = PartialTransformer.define[Source, TargetExtra].withFieldConst(_.d, 0L)
      val t1 = base.withFieldComputed(_.a, _.a + 1).buildTransformer
      val t2 = base.withFieldComputed(_.a, _.a + 2).buildTransformer
      t1.transform(src).asOption ==> Some(TargetExtra(2, "hello", 3.14, 0L))
      t2.transform(src).asOption ==> Some(TargetExtra(3, "hello", 3.14, 0L))
    }

    test("deep chain from val after branching") {
      val base = PartialTransformer.define[Source, TargetExtra].withFieldConst(_.d, 1L)
      val t = base
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "x")
        .buildTransformer
      t.transform(src).asOption ==> Some(TargetExtra(10, "x", 3.14, 1L))
      val t2 = base.buildTransformer
      t2.transform(src).asOption ==> Some(TargetExtra(1, "hello", 3.14, 1L))
    }
  }
}
