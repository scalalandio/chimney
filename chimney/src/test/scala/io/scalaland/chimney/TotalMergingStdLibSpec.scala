package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.collection.immutable.{ListMap, ListSet}

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

    test("should use only source Either when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .transform ==> Right("10")
      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .transform ==> Right("10")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .transform ==> Left("0")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .transform ==> Left("0")

      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .transform ==> Nested(Left("0"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .transform ==> Nested(Left("0"))
    }

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      List(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(ListSet(6, 7, 8, 9, 10))
        .transform ==> Vector("1", "2", "3", "4", "5")
      Vector(1, 2, 3, 4, 5)
        .into[ListSet[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .transform ==> ListSet("1", "2", "3", "4", "5")
      ListSet(1, 2, 3, 4, 5)
        .into[List[String]]
        .withFallback(Vector(6, 7, 8, 9, 10))
        .transform ==> List("1", "2", "3", "4", "5")

      Nested(List(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(ListSet(6, 7, 8, 9, 10)))
        .transform ==> Nested(Vector("1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[ListSet[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .transform ==> Nested(ListSet("1", "2", "3", "4", "5"))
      Nested(ListSet(1, 2, 3, 4, 5))
        .into[Nested[List[String]]]
        .withFallback(Nested(Vector(6, 7, 8, 9, 10)))
        .transform ==> Nested(List("1", "2", "3", "4", "5"))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      ListMap(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")

      Nested(Map(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
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

    test("should use only source Either when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallbackFrom(_.value)(Either.cond(true, "fallback", "fail"))
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallbackFrom(_.value)(Either.cond(false, "fallback", "fail"))
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallbackFrom(_.value)(Either.cond(true, "fallback", "fail"))
        .transform ==> Nested(Left("0"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallbackFrom(_.value)(Either.cond(false, "fallback", "fail"))
        .transform ==> Nested(Left("0"))

      Nested(Nested(Either.cond(true, 10, 0)))
        .into[Nested[Nested[Either[String, String]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(true, "fallback", "fail")))
        .transform ==> Nested(Nested(Right("10")))
      Nested(Nested(Either.cond(true, 10, 0)))
        .into[Nested[Nested[Either[String, String]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(false, "fallback", "fail")))
        .transform ==> Nested(Nested(Right("10")))
      Nested(Nested(Either.cond(false, 10, 0)))
        .into[Nested[Nested[Either[String, String]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(true, "fallback", "fail")))
        .transform ==> Nested(Nested(Left("0")))
      Nested(Nested(Either.cond(false, 10, 0)))
        .into[Nested[Nested[Either[String, String]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(false, "fallback", "fail")))
        .transform ==> Nested(Nested(Left("0")))
    }

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(List(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallbackFrom(_.value)(ListSet(6, 7, 8, 9, 10))
        .transform ==> Nested(Vector("1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[ListSet[String]]]
        .withFallbackFrom(_.value)(List(6, 7, 8, 9, 10))
        .transform ==> Nested(ListSet("1", "2", "3", "4", "5"))
      Nested(ListSet(1, 2, 3, 4, 5))
        .into[Nested[List[String]]]
        .withFallbackFrom(_.value)(Vector(6, 7, 8, 9, 10))
        .transform ==> Nested(List("1", "2", "3", "4", "5"))

      Nested(Nested(List(1, 2, 3, 4, 5)))
        .into[Nested[Nested[Vector[String]]]]
        .withFallbackFrom(_.value)(Nested(ListSet(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(Vector("1", "2", "3", "4", "5")))
      Nested(Nested(Vector(1, 2, 3, 4, 5)))
        .into[Nested[Nested[ListSet[String]]]]
        .withFallbackFrom(_.value)(Nested(List(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(ListSet("1", "2", "3", "4", "5")))
      Nested(Nested(ListSet(1, 2, 3, 4, 5)))
        .into[Nested[Nested[List[String]]]]
        .withFallbackFrom(_.value)(Nested(Vector(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(List("1", "2", "3", "4", "5")))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallbackFrom(_.value)(ListMap(5 -> 6, 7 -> 8))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")

      Nested(Nested(Map(1 -> 2, 3 -> 4)))
        .into[Nested[Nested[ListMap[String, String]]]]
        .withFallbackFrom(_.value)(Nested(ListMap(5 -> 6, 7 -> 8)))
        .transform
        .value
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
    }
  }

  group("flag .enableOptionFallbackMerge(optionFallbackMergeStrategy)") {

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

  group("flag .enableEitherFallbackMerge(optionFallbackMergeStrategy)") {

    test("should merge Eithers from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Right("10")
      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Right("10")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Right("fallback")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Left("fail")

      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Right("fallback"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform ==> Nested(Left("fail"))
    }

    test("should merge Eithers from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Right("fallback")
      Either
        .cond(true, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Right("10")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(true, "fallback", "fail"))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Right("fallback")
      Either
        .cond(false, 10, 0)
        .into[Either[String, String]]
        .withFallback(Either.cond(false, "fallback", "fail"))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Left("0")

      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Right("fallback"))
      Nested(Either.cond(true, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Right("10"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(true, "fallback", "fail")))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Right("fallback"))
      Nested(Either.cond(false, 10, 0))
        .into[Nested[Either[String, String]]]
        .withFallback(Nested(Either.cond(false, "fallback", "fail")))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform ==> Nested(Left("0"))
    }
  }

  group("flag .enableCollectionFallbackMerge(collectionFallbackMergeStrategy)") {

    test("should merge sequential-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      List(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(ListSet(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Vector("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      Vector(1, 2, 3, 4, 5)
        .into[ListSet[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> ListSet("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      ListSet(1, 2, 3, 4, 5)
        .into[List[String]]
        .withFallback(Vector(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

      Nested(List(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(ListSet(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(Vector("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[ListSet[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(ListSet("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
      Nested(ListSet(1, 2, 3, 4, 5))
        .into[Nested[List[String]]]
        .withFallback(Nested(Vector(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    }

    test("should merge Map-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      ListMap(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")

      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
    }

    test("should merge sequential-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      List(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(ListSet(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Vector("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")
      Vector(1, 2, 3, 4, 5)
        .into[ListSet[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> ListSet("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")
      ListSet(1, 2, 3, 4, 5)
        .into[List[String]]
        .withFallback(Vector(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> List("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")

      Nested(List(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(ListSet(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(Vector("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[ListSet[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(ListSet("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
      Nested(ListSet(1, 2, 3, 4, 5))
        .into[Nested[List[String]]]
        .withFallback(Nested(Vector(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(List("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
    }

    test("should merge Map-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      ListMap(1 -> 2, 3 -> 4)
        .into[Map[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")

      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[Map[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .value
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
    }
  }
}
