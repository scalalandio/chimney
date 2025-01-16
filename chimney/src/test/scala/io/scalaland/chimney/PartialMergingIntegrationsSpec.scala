package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialMergingIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*

  group("setting .withFallback(fallbackValue)") {

    test("should use only source Possible when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Possible[Int]]
        .withFallback(20)
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .transform
        .asOption ==> Some(Possible.Present(10))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .transform
        .asOption ==> Some(Possible.Nope)
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .transform
        .asOption ==> Some(Possible.Nope)

      Nested("10")
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(20))
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should use only source Possible when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested("10")
        .intoPartial[Nested[Possible[Int]]]
        .withFallbackFrom(_.value)(20)
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallbackFrom(_.value)(Possible(20))
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallbackFrom(_.value)(Possible.Nope: Possible[String])
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallbackFrom(_.value)(Possible(20))
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallbackFrom(_.value)(Possible.Nope: Possible[Int])
        .transform
        .asOption ==> Some(Nested(Possible.Nope))

      Nested(Nested("10"))
        .intoPartial[Nested[Nested[Possible[Int]]]]
        .withFallbackFrom(_.value)(Nested(20))
        .transform
        .asOption ==> Some(Nested(Nested(Possible.Present(10))))
      Nested(Nested(Possible("10")))
        .intoPartial[Nested[Nested[Possible[Int]]]]
        .withFallbackFrom(_.value)(Nested(Possible(20)))
        .transform
        .asOption ==> Some(Nested(Nested(Possible.Present(10))))
      Nested(Nested(Possible("10")))
        .intoPartial[Nested[Nested[Possible[Int]]]]
        .withFallbackFrom(_.value)(Nested(Possible.Nope: Possible[Int]))
        .transform
        .asOption ==> Some(Nested(Nested(Possible.Present(10))))
      Nested(Nested(Possible.Nope: Possible[String]))
        .intoPartial[Nested[Nested[Possible[Int]]]]
        .withFallbackFrom(_.value)(Nested(Possible(20)))
        .transform
        .asOption ==> Some(Nested(Nested(Possible.Nope)))
      Nested(Nested(Possible.Nope: Possible[String]))
        .intoPartial[Nested[Nested[Possible[Int]]]]
        .withFallbackFrom(_.value)(Nested(Possible.Nope: Possible[Int]))
        .transform
        .asOption ==> Some(Nested(Nested(Possible.Nope)))
    }
  }

  group("flag .enableOptionFallbackMerge(SourceOrElseFallback)") {

    test("should merge Possibles from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Possible[Int]]
        .withFallback(20)
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Possible.Present(10))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Possible.Present(20))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Possible.Nope)

      Nested("10")
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Possible.Present(20)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
    }

    test("should merge Possibles from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Possible[Int]]
        .withFallback(20)
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Possible.Present(20))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Possible.Present(20))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Possible.Present(10))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Possible.Present(20))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Possible.Nope)

      Nested("10")
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Possible.Present(20)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Possible.Present(20)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Possible.Present(20)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
    }
  }
}
