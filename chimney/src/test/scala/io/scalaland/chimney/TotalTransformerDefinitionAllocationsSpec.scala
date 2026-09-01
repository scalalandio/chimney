package io.scalaland.chimney

/** Verifies that [[dsl.TransformerDefinition]] chains with various combinations of data-carrying and type-only modifiers
  * produce correct transformations — both from fresh expressions and from `val` references (branching).
  */
class TotalTransformerDefinitionAllocationsSpec extends ChimneySpec {

  case class Source(a: Int, b: String, c: Double)
  case class Target(a: Int, b: String, c: Double)
  case class TargetRenamed(a: Int, bb: String, c: Double)
  case class TargetExtra(a: Int, b: String, c: Double, d: Long)
  case class TargetFewer(a: Int, c: Double)

  val src = Source(1, "hello", 3.14)

  group("fresh expression chains") {

    test("0 data-carrying modifiers (empty chain)") {
      val t = Transformer.define[Source, Target].buildTransformer
      t.transform(src) ==> Target(1, "hello", 3.14)
    }

    test("1 data-carrying modifier (withFieldConst)") {
      val t = Transformer.define[Source, TargetExtra].withFieldConst(_.d, 42L).buildTransformer
      t.transform(src) ==> TargetExtra(1, "hello", 3.14, 42L)
    }

    test("multiple data-carrying modifiers (withFieldConst + withFieldComputed)") {
      val t = Transformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 100L)
        .withFieldComputed(_.a, s => s.a + 10)
        .buildTransformer
      t.transform(src) ==> TargetExtra(11, "hello", 3.14, 100L)
    }

    test("data-carrying modifier interleaved with type-only modifier (withFieldRenamed)") {
      val t = Transformer
        .define[Source, TargetRenamed]
        .withFieldRenamed(_.b, _.bb)
        .buildTransformer
      t.transform(src) ==> TargetRenamed(1, "hello", 3.14)
    }

    test("data-carrying + type-only interleaved") {
      val t = Transformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 99L)
        .withFieldRenamed(_.b, _.b) // identity rename, just a type-only modifier in the chain
        .withFieldComputed(_.c, s => s.c * 2)
        .buildTransformer
      t.transform(src) ==> TargetExtra(1, "hello", 6.28, 99L)
    }

    test("multiple type-only modifiers between data modifiers") {
      val t = Transformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 1L)
        .enableMethodAccessors
        .disableMethodAccessors
        .withFieldConst(_.a, 999)
        .buildTransformer
      t.transform(src) ==> TargetExtra(999, "hello", 3.14, 1L)
    }

    test("only type-only modifiers (enableMethodAccessors etc.)") {
      val t = Transformer
        .define[Source, Target]
        .enableMethodAccessors
        .disableMethodAccessors
        .buildTransformer
      t.transform(src) ==> Target(1, "hello", 3.14)
    }

    test("three consecutive data-carrying modifiers") {
      val t = Transformer
        .define[Source, TargetExtra]
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "world")
        .withFieldConst(_.d, 7L)
        .buildTransformer
      t.transform(src) ==> TargetExtra(10, "world", 3.14, 7L)
    }

    test("withFieldConst overriding a previous withFieldConst for same field") {
      val t = Transformer
        .define[Source, TargetExtra]
        .withFieldConst(_.d, 1L)
        .withFieldConst(_.d, 2L)
        .buildTransformer
      t.transform(src) ==> TargetExtra(1, "hello", 3.14, 2L)
    }

    test("withFieldUnused (type-only) + withFieldConst (data)") {
      val t = Transformer
        .define[Source, TargetFewer]
        .withFieldUnused(_.b)
        .withFieldConst(_.c, 0.0)
        .buildTransformer
      t.transform(src) ==> TargetFewer(1, 0.0)
    }
  }

  group("val reference chains (branching)") {

    test("shared base with 0 data modifiers, then branching") {
      val base = Transformer.define[Source, TargetExtra]
      val t1 = base.withFieldConst(_.d, 10L).buildTransformer
      val t2 = base.withFieldConst(_.d, 20L).buildTransformer
      t1.transform(src) ==> TargetExtra(1, "hello", 3.14, 10L)
      t2.transform(src) ==> TargetExtra(1, "hello", 3.14, 20L)
    }

    test("shared base with 1 data modifier, then branching with more data") {
      val base = Transformer.define[Source, TargetExtra].withFieldConst(_.d, 0L)
      val t1 = base.withFieldComputed(_.a, _.a + 1).buildTransformer
      val t2 = base.withFieldComputed(_.a, _.a + 2).buildTransformer
      t1.transform(src) ==> TargetExtra(2, "hello", 3.14, 0L)
      t2.transform(src) ==> TargetExtra(3, "hello", 3.14, 0L)
    }

    test("shared base with data, then one branch adds data and other adds type-only") {
      val base = Transformer.define[Source, TargetExtra].withFieldConst(_.d, 5L)
      val t1 = base.withFieldComputed(_.a, _ => 100).buildTransformer
      val t2 = base.enableMethodAccessors.buildTransformer
      t1.transform(src) ==> TargetExtra(100, "hello", 3.14, 5L)
      t2.transform(src) ==> TargetExtra(1, "hello", 3.14, 5L)
    }

    test("deep chain from val: 3+ data modifiers after branching") {
      val base = Transformer.define[Source, TargetExtra].withFieldConst(_.d, 1L)
      val t = base
        .withFieldConst(_.a, 10)
        .withFieldConst(_.b, "x")
        .withFieldComputed(_.c, _ => 0.0)
        .buildTransformer
      t.transform(src) ==> TargetExtra(10, "x", 0.0, 1L)
      // original base still works independently
      val t2 = base.buildTransformer
      t2.transform(src) ==> TargetExtra(1, "hello", 3.14, 1L)
    }
  }
}
