package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalMergingStdLibSpec extends ChimneySpec {

  group("setting .withFallback(fallbackValue)") {

    test("should use only source Option when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10
        .into[Option[String]]
        .withFallback("fallback")
        .transform ==> Some("10")
      Option(10)
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .transform ==> Some("10")
      Option(10)
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .transform ==> Some("10")
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .transform ==> None
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .transform ==> None

      Nested(10)
        .into[Nested[Option[String]]]
        .withFallback(Nested("fallback"))
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .transform ==> Nested(Some("10"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .transform ==> Nested(None)
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .transform ==> Nested(None)
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should use only source Option when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(10)
        .into[Nested[Option[String]]]
        .withFallbackFrom(_.value)("fallback")
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallbackFrom(_.value)(Option("fallback"))
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallbackFrom(_.value)(Option.empty[String])
        .transform ==> Nested(Some("10"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallbackFrom(_.value)(Option("fallback"))
        .transform ==> Nested(None)
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallbackFrom(_.value)(Option.empty[String])
        .transform ==> Nested(None)

      Nested(Nested(10))
        .into[Nested[Nested[Option[String]]]]
        .withFallbackFrom(_.value)(Nested("fallback"))
        .transform ==> Nested(Nested(Some("10")))
      Nested(Nested(Option(10)))
        .into[Nested[Nested[Option[String]]]]
        .withFallbackFrom(_.value)(Nested(Option("fallback")))
        .transform ==> Nested(Nested(Some("10")))
      Nested(Nested(Option(10)))
        .into[Nested[Nested[Option[String]]]]
        .withFallbackFrom(_.value)(Nested(Option.empty[String]))
        .transform ==> Nested(Nested(Some("10")))
      Nested(Nested(Option.empty[Int]))
        .into[Nested[Nested[Option[String]]]]
        .withFallbackFrom(_.value)(Nested(Option("fallback")))
        .transform ==> Nested(Nested(None))
      Nested(Nested(Option.empty[Int]))
        .into[Nested[Nested[Option[String]]]]
        .withFallbackFrom(_.value)(Nested(Option.empty[String]))
        .transform ==> Nested(Nested(None))
    }
  }

  group("flag .enableOptionFallbackMerge(SourceOrElseFallback)") {

    test("should merge Options from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10.into[Option[String]]
        .withFallback("fallback")
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Some("10")
      Option(10)
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Some("10")
      Option(10)
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Some("10")
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Some("fallback")
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> None

      Nested(10)
        .into[Nested[Option[String]]]
        .withFallback(Nested("fallback"))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Some("10"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Some("10"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Some("fallback"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(None)
    }

    test("should merge Options from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      10.into[Option[String]]
        .withFallback("fallback")
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Some("fallback")
      Option(10)
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Some("fallback")
      Option(10)
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Some("10")
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Some("fallback")
      Option
        .empty[Int]
        .into[Option[String]]
        .withFallback(Option.empty[String])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> None

      Nested(10)
        .into[Nested[Option[String]]]
        .withFallback(Nested("fallback"))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Some("fallback"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Some("fallback"))
      Nested(Option(10))
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Some("10"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option("fallback")))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Some("fallback"))
      Nested(Option.empty[Int])
        .into[Nested[Option[String]]]
        .withFallback(Nested(Option.empty[String]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(None)
    }
  }
}
