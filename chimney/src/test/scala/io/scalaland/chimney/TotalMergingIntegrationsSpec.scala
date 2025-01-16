package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalMergingIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*

  group("setting .withFallback(fallbackValue)") {

    test("should use only source Possible when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10
        .into[Possible[String]]
        .withFallback("fallback")
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .transform ==> Possible.Present("10")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .transform ==> Possible.Nope
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .transform ==> Possible.Nope

      Nested(10)
        .into[Nested[Possible[String]]]
        .withFallback(Nested("fallback"))
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .transform ==> Nested(Possible.Nope)
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .transform ==> Nested(Possible.Nope)
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should use only source Possible when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(10)
        .into[Nested[Possible[String]]]
        .withFallbackFrom(_.value)("fallback")
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallbackFrom(_.value)(Possible("fallback"))
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallbackFrom(_.value)(Possible.Nope: Possible[String])
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallbackFrom(_.value)(Possible("fallback"))
        .transform ==> Nested(Possible.Nope)
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallbackFrom(_.value)(Possible.Nope: Possible[String])
        .transform ==> Nested(Possible.Nope)

      Nested(Nested(10))
        .into[Nested[Nested[Possible[String]]]]
        .withFallbackFrom(_.value)(Nested("fallback"))
        .transform ==> Nested(Nested(Possible.Present("10")))
      Nested(Nested(Possible(10)))
        .into[Nested[Nested[Possible[String]]]]
        .withFallbackFrom(_.value)(Nested(Possible("fallback")))
        .transform ==> Nested(Nested(Possible.Present("10")))
      Nested(Nested(Possible(10)))
        .into[Nested[Nested[Possible[String]]]]
        .withFallbackFrom(_.value)(Nested(Possible.Nope: Possible[String]))
        .transform ==> Nested(Nested(Possible.Present("10")))
      Nested(Nested(Possible.Nope: Possible[Int]))
        .into[Nested[Nested[Possible[String]]]]
        .withFallbackFrom(_.value)(Nested(Possible("fallback")))
        .transform ==> Nested(Nested(Possible.Nope))
      Nested(Nested(Possible.Nope: Possible[Int]))
        .into[Nested[Nested[Possible[String]]]]
        .withFallbackFrom(_.value)(Nested(Possible.Nope: Possible[String]))
        .transform ==> Nested(Nested(Possible.Nope))
    }
  }

  group("flag .enableOptionFallbackMerge(SourceOrElseFallback)") {

    test("should merge Possibles from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10.into[Possible[String]]
        .withFallback("fallback")
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Possible.Present("10")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Possible.Present("fallback")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Possible.Nope

      Nested(10)
        .into[Nested[Possible[String]]]
        .withFallback(Nested("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Possible.Present("fallback"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Possible.Nope)
    }

    test("should merge Possibles from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10.into[Possible[String]]
        .withFallback("fallback")
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Possible.Present("fallback")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Possible.Present("fallback")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Possible.Present("10")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Possible.Present("fallback")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Possible.Nope

      Nested(10)
        .into[Nested[Possible[String]]]
        .withFallback(Nested("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Possible.Present("fallback"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Possible.Present("fallback"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Possible.Present("fallback"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Possible.Nope)
    }
  }
}
