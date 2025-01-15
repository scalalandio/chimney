package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialMergingStdLibSpec extends ChimneySpec {

  group("setting .withFallback(fallbackValue)") {

    test("should use only source Option when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Option[Int]]
        .withFallback(20)
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .transform
        .asOption ==> Some(Some(10))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .transform
        .asOption ==> Some(None)
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .transform
        .asOption ==> Some(None)

      Nested("10")
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(20))
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[String]))
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .transform
        .asOption ==> Some(Nested(None))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .transform
        .asOption ==> Some(Nested(None))
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should use only source Option when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested("10")
        .intoPartial[Nested[Option[Int]]]
        .withFallbackFrom(_.value)(20)
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallbackFrom(_.value)(Option(20))
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallbackFrom(_.value)(Option.empty[String])
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallbackFrom(_.value)(Option(20))
        .transform
        .asOption ==> Some(Nested(None))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallbackFrom(_.value)(Option.empty[Int])
        .transform
        .asOption ==> Some(Nested(None))

      Nested(Nested("10"))
        .intoPartial[Nested[Nested[Option[Int]]]]
        .withFallbackFrom(_.value)(Nested(20))
        .transform
        .asOption ==> Some(Nested(Nested(Some(10))))
      Nested(Nested(Option("10")))
        .intoPartial[Nested[Nested[Option[Int]]]]
        .withFallbackFrom(_.value)(Nested(Option(20)))
        .transform
        .asOption ==> Some(Nested(Nested(Some(10))))
      Nested(Nested(Option("10")))
        .intoPartial[Nested[Nested[Option[Int]]]]
        .withFallbackFrom(_.value)(Nested(Option.empty[Int]))
        .transform
        .asOption ==> Some(Nested(Nested(Some(10))))
      Nested(Nested(Option.empty[String]))
        .intoPartial[Nested[Nested[Option[Int]]]]
        .withFallbackFrom(_.value)(Nested(Option(20)))
        .transform
        .asOption ==> Some(Nested(Nested(None)))
      Nested(Nested(Option.empty[String]))
        .intoPartial[Nested[Nested[Option[Int]]]]
        .withFallbackFrom(_.value)(Nested(Option.empty[Int]))
        .transform
        .asOption ==> Some(Nested(Nested(None)))
    }
  }

  group("flag .enableOptionFallbackMerge(SourceOrElseFallback)") {

    test("should merge Options from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Option[Int]]
        .withFallback(20)
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[String])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Some(10))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Some(20))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(None)

      Nested("10")
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(20))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Some(20)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(None))
    }

    test("should merge Options from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      "10"
        .intoPartial[Option[Int]]
        .withFallback(20)
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Some(20))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Some(20))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Some(10))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Some(20))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(None)

      Nested("10")
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(20))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Some(20)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Some(20)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Some(20)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(None))
    }
  }
}
