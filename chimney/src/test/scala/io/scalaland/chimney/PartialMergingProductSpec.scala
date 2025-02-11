package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialMergingProductSpec extends ChimneySpec {

  group("setting .withFallback(fallbackValue)") {

    test("should merge 2 case classes with disjoint field names") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0)
        .intoPartial[Baz[Double]]
        .withFallback(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Baz(1, "b", 3.0, 4, "e", 6.0))

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))
    }

    test("should merge 2 case classes with overlapping field names") {
      import merges.Overlapping.*, merges.Nested

      Foo(1, 2.0)
        .intoPartial[Baz[Double]]
        .withFallback(Bar("3", 4.0))
        .transform
        .asOption ==> Some(Baz(1, "3", 2.0))

      Nested(Foo(1, 2.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallback(Nested(Bar("3", 4.0)))
        .transform
        .asOption ==> Some(Nested(Baz(1, "3", 2.0)))
    }

    test("should merge cases classes (with at least 1 tuple) into case class") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0)
        .intoPartial[Baz[Double]]
        .withFallback((4, "e", 6.0))
        .transform
        .asOption ==> Some(Baz(1, "b", 3.0, 4, "e", 6.0))
      (1, "b", 3.0)
        .intoPartial[Baz[Double]]
        .withFallback(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Baz(1, "b", 3.0, 4, "e", 6.0))

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallback(Nested((4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))
      Nested((1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))
    }

    test("should merge cases classes (with at least 1 tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0)
        .intoPartial[(Int, String, Double, Int, String, Double)]
        .withFallback((4, "e", 6.0))
        .transform
        .asOption ==> Some((1, "b", 3.0, 4, "e", 6.0))
      (1, "b", 3.0)
        .intoPartial[(Int, String, Double, Int, String, Double)]
        .withFallback(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some((1, "b", 3.0, 4, "e", 6.0))

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallback(Nested((4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))
      Nested((1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))
    }

    test("should merge cases classes (none of them a tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0)
        .intoPartial[(Int, String, Double, Int, String, Double)]
        .withFallback(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some((1, "b", 3.0, 4, "e", 6.0))

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))
    }

    test("should merge cases classes (none of them a tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      compileErrors(
        """
        Foo(1, "b", 3.0)
          .intoPartial[(Int, String, Double, Int, String, Double, Long)]
          .withFallback(Bar(4, "e", 6.0))
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] to scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]",
        "scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]",
        "  source tuple io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] is of arity 3 (with fallbacks of arities: 3), while target type scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] is of arity 7; source should be at least as big as target!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors(
        """
        Nested(Foo(1, "b", 3.0))
          .intoPartial[Nested[(Int, String, Double, Int, String, Double, Long)]]
          .withFallback(Nested(Bar(4, "e", 6.0)))
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]] to io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]",
        "io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]",
        "  value: scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] - can't derive transformation from value: io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]]",
        "scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]",
        "  source tuple io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] is of arity 3 (with fallbacks of arities: 3), while target type scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] is of arity 7; source should be at least as big as target!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should merge cases classes cooperating with other overloads") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0)
        .intoPartial[(Int, String, Double, Int, String, Double, Int)]
        .withFallback(Bar(4, "e", 6.0))
        .withFieldConst(_._2, "const")
        .withFieldComputed(_._5, _ => "computed")
        .withFieldRenamed(_.a, _._7)
        .transform
        .asOption ==> Some((1, "const", 3.0, 4, "computed", 6.0, 1))

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double, Int)]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .withFieldConst(_.value._2, "const")
        .withFieldComputed(_.value._5, _ => "computed")
        .withFieldRenamed(_.value.a, _.value._7)
        .transform
        .asOption ==> Some(Nested((1, "const", 3.0, 4, "computed", 6.0, 1)))
    }

    test("should work with semiautomatic derivation") {
      import merges.Disjoint.*, merges.Nested

      PartialTransformer
        .define[Foo[Double], Baz[Double]]
        .withFallback(Bar(4, "e", 6.0))
        .buildTransformer
        .transform(Foo(1, "b", 3.0))
        .asOption ==> Some(Baz(1, "b", 3.0, 4, "e", 6.0))

      PartialTransformer
        .define[Nested[Foo[Double]], Nested[Baz[Double]]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .buildTransformer
        .transform(Nested(Foo(1, "b", 3.0)))
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should merge 2 case classes with disjoint field names") {
      import merges.Disjoint.*, merges.Nested

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))

      Nested(Nested(Foo(1, "b", 3.0)))
        .intoPartial[Nested[Nested[Baz[Double]]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz(1, "b", 3.0, 4, "e", 6.0))))
    }

    test("should merge 2 case classes with overlapping field names") {
      import merges.Overlapping.*, merges.Nested

      Nested(Foo(1, 2.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallbackFrom(_.value)(Bar("3", 4.0))
        .transform
        .asOption ==> Some(Nested(Baz(1, "3", 2.0)))

      Nested(Nested(Foo(1, 2.0)))
        .intoPartial[Nested[Nested[Baz[Double]]]]
        .withFallbackFrom(_.value)(Nested(Bar("3", 4.0)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz(1, "3", 2.0))))
    }

    test("should merge cases classes (with at least 1 tuple) into case class") {
      import merges.Disjoint.*, merges.Nested

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallbackFrom(_.value)((4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))
      Nested((1, "b", 3.0))
        .intoPartial[Nested[Baz[Double]]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))

      Nested(Nested(Foo(1, "b", 3.0)))
        .intoPartial[Nested[Nested[Baz[Double]]]]
        .withFallbackFrom(_.value)(Nested((4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz(1, "b", 3.0, 4, "e", 6.0))))
      Nested(Nested((1, "b", 3.0)))
        .intoPartial[Nested[Nested[Baz[Double]]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz(1, "b", 3.0, 4, "e", 6.0))))
    }

    test("should merge cases classes (with at least 1 tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallbackFrom(_.value)((4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))
      Nested((1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))

      Nested(Nested(Foo(1, "b", 3.0)))
        .intoPartial[Nested[Nested[(Int, String, Double, Int, String, Double)]]]
        .withFallbackFrom(_.value)(Nested((4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested((1, "b", 3.0, 4, "e", 6.0))))
      Nested(Nested((1, "b", 3.0)))
        .intoPartial[Nested[Nested[(Int, String, Double, Int, String, Double)]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested((1, "b", 3.0, 4, "e", 6.0))))
    }

    test("should merge cases classes (none of them a tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double)]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .transform
        .asOption ==> Some(Nested((1, "b", 3.0, 4, "e", 6.0)))

      Nested(Nested(Foo(1, "b", 3.0)))
        .intoPartial[Nested[Nested[(Int, String, Double, Int, String, Double)]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .transform
        .asOption ==> Some(Nested(Nested((1, "b", 3.0, 4, "e", 6.0))))
    }

    test("should merge cases classes (none of them a tuple) into tuple") {
      import merges.Disjoint.*, merges.Nested

      compileErrors(
        """
        Nested(Foo(1, "b", 3.0))
          .intoPartial[Nested[(Int, String, Double, Int, String, Double, Long)]]
          .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]] to io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]",
        "io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]",
        "  value: scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] - can't derive transformation from value: io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]]",
        "scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] (transforming from: value into: value)",
        "  source tuple io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] is of arity 3 (with fallbacks of arities: 3), while target type scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] is of arity 7; source should be at least as big as target!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors(
        """
        Nested(Nested(Foo(1, "b", 3.0)))
          .intoPartial[Nested[Nested[(Int, String, Double, Int, String, Double, Long)]]]
          .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]]] to io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]]",
        "io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]]]",
        "  value: io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]] - can't derive transformation from value: io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]] in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]]]",
        "io.scalaland.chimney.fixtures.merges.Nested[scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long]] (transforming from: value into: value)",
        "  value: scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] - can't derive transformation from value: io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double]]",
        "scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] (transforming from: value.value into: value.value)",
        "  source tuple io.scalaland.chimney.fixtures.merges.Disjoint.Foo[scala.Double] is of arity 3 (with fallbacks of arities: 3), while target type scala.Tuple7[scala.Int, java.lang.String, scala.Double, scala.Int, java.lang.String, scala.Double, scala.Long] is of arity 7; source should be at least as big as target!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should merge cases classes cooperating with other overloads") {
      import merges.Disjoint.*, merges.Nested

      Nested(Foo(1, "b", 3.0))
        .intoPartial[Nested[(Int, String, Double, Int, String, Double, Int)]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .withFieldConst(_.value._2, "const")
        .withFieldComputed(_.value._5, _ => "computed")
        .withFieldRenamed(_.value.a, _.value._7)
        .transform
        .asOption ==> Some(Nested((1, "const", 3.0, 4, "computed", 6.0, 1)))

      Nested(Nested(Foo(1, "b", 3.0)))
        .intoPartial[Nested[Nested[(Int, String, Double, Int, String, Double, Int)]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .withFieldConst(_.value.value._2, "const")
        .withFieldComputedFrom(_.value)(_.value.value._5, _ => "computed")
        .withFieldRenamed(_.value.value.a, _.value.value._7)
        .transform
        .asOption ==> Some(Nested(Nested((1, "const", 3.0, 4, "computed", 6.0, 1))))
    }

    test("should work with semiautomatic derivation") {
      import merges.Disjoint.*, merges.Nested

      PartialTransformer
        .define[Nested[Foo[Double]], Nested[Baz[Double]]]
        .withFallbackFrom(_.value)(Bar(4, "e", 6.0))
        .buildTransformer
        .transform(Nested(Foo(1, "b", 3.0)))
        .asOption ==> Some(Nested(Baz(1, "b", 3.0, 4, "e", 6.0)))

      PartialTransformer
        .define[Nested[Nested[Foo[Double]]], Nested[Nested[Baz[Double]]]]
        .withFallbackFrom(_.value)(Nested(Bar(4, "e", 6.0)))
        .buildTransformer
        .transform(Nested(Nested(Foo(1, "b", 3.0))))
        .asOption ==> Some(Nested(Nested(Baz(1, "b", 3.0, 4, "e", 6.0))))
    }
  }
}
