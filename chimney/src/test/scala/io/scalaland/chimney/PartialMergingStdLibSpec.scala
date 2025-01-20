package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.collection.immutable.{ListMap, ListSet}

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

    test("should use only source Either when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, -1, 20))
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, -1, 20))
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, -1, 20))
        .transform
        .asOption ==> Some(Left(0))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, -1, 20))
        .transform
        .asOption ==> Some(Left(0))

      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Left(0)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Left(0)))
    }

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      List("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(ListSet("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[ListSet[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(ListSet(1, 2, 3, 4, 5))
      ListSet("1", "2", "3", "4", "5")
        .intoPartial[List[Int]]
        .withFallback(Vector("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(List(1, 2, 3, 4, 5))

      Nested(List("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(ListSet("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[ListSet[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(ListSet(1, 2, 3, 4, 5)))
      Nested(ListSet("1", "2", "3", "4", "5"))
        .intoPartial[Nested[List[Int]]]
        .withFallback(Nested(Vector("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(List(1, 2, 3, 4, 5)))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      ListMap("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
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

    test("should use only source Either when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallbackFrom(_.value)(Either.cond(true, -1, 20))
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallbackFrom(_.value)(Either.cond(false, -1, 20))
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallbackFrom(_.value)(Either.cond(true, -1, 20))
        .transform
        .asOption ==> Some(Nested(Left(0)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallbackFrom(_.value)(Either.cond(false, -1, 20))
        .transform
        .asOption ==> Some(Nested(Left(0)))

      Nested(Nested(Either.cond(true, "10", "0")))
        .intoPartial[Nested[Nested[Either[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(true, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Nested(Right(10))))
      Nested(Nested(Either.cond(true, "10", "0")))
        .intoPartial[Nested[Nested[Either[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(false, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Nested(Right(10))))
      Nested(Nested(Either.cond(false, "10", "0")))
        .intoPartial[Nested[Nested[Either[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(true, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Nested(Left(0))))
      Nested(Nested(Either.cond(false, "10", "0")))
        .intoPartial[Nested[Nested[Either[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(Either.cond(false, -1, 20)))
        .transform
        .asOption ==> Some(Nested(Nested(Left(0))))
    }

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested(List("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallbackFrom(_.value)(ListSet("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[ListSet[Int]]]
        .withFallbackFrom(_.value)(List("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(ListSet(1, 2, 3, 4, 5)))
      Nested(ListSet("1", "2", "3", "4", "5"))
        .intoPartial[Nested[List[Int]]]
        .withFallbackFrom(_.value)(Vector("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(List(1, 2, 3, 4, 5)))

      Nested(Nested(List("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[Vector[Int]]]]
        .withFallbackFrom(_.value)(Nested(ListSet("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(Vector(1, 2, 3, 4, 5))))
      Nested(Nested(Vector("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[ListSet[Int]]]]
        .withFallbackFrom(_.value)(Nested(List("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(ListSet(1, 2, 3, 4, 5))))
      Nested(Nested(ListSet("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[List[Int]]]]
        .withFallbackFrom(_.value)(Nested(Vector("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(List(1, 2, 3, 4, 5))))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallbackFrom(_.value)(ListMap("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(Nested(ListMap("1" -> "2", "3" -> "4")))
        .intoPartial[Nested[Nested[ListMap[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(ListMap("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
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

  group("flag .disableOptionFallbackMerge") {

    test("should disable globally enabled .enableOptionFallbackMerge") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      implicit val cfg = TransformerConfiguration.default.enableOptionFallbackMerge(SourceOrElseFallback)

      "10"
        .intoPartial[Option[Int]]
        .withFallback(20)
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Some(10))
      Option("10")
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Some(10))
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(None)
      Option
        .empty[String]
        .intoPartial[Option[Int]]
        .withFallback(Option.empty[Int])
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(None)

      Nested("10")
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option("10"))
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[String]))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Some(10)))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option(20)))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(None))
      Nested(Option.empty[String])
        .intoPartial[Nested[Option[Int]]]
        .withFallback(Nested(Option.empty[Int]))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(None))
    }
  }

  group("flag .enableEitherFallbackMerge(optionFallbackMergeStrategy)") {

    test("should merge Eithers from source to fallback when SourceOrElseFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, 20, -1))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, 20, -1))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, 20, -1))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Right(20))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, 20, -1))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Left(-1))

      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, 20, -1)))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, 20, -1)))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, 20, -1)))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Right(20)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, 20, -1)))
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
        .asOption ==> Some(Nested(Left(-1)))
    }

    test("should merge Eithers from fallback to source when FallbackOrElseSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, 20, -1))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Right(20))
      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, 20, -1))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, 20, -1))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Right(20))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, 20, -1))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Left(0))

      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, 20, -1)))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Right(20)))
      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, 20, -1)))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, 20, -1)))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Right(20)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, 20, -1)))
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
        .asOption ==> Some(Nested(Left(0)))
    }
  }

  group("flag .disableOptionFallbackMerge") {

    test("should disable globally enabled .enableOptionFallbackMerge") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      implicit val cfg = TransformerConfiguration.default.enableEitherFallbackMerge(SourceOrElseFallback)

      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, -1, 20))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(true, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, -1, 20))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Right(10))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(true, -1, 20))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Left(0))
      Either
        .cond(false, "10", "0")
        .intoPartial[Either[Int, Int]]
        .withFallback(Either.cond(false, -1, 20))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Left(0))

      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, -1, 20)))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(true, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, -1, 20)))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Nested(Right(10)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(true, -1, 20)))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Nested(Left(0)))
      Nested(Either.cond(false, "10", "0"))
        .intoPartial[Nested[Either[Int, Int]]]
        .withFallback(Nested(Either.cond(false, -1, 20)))
        .disableEitherFallbackMerge
        .transform
        .asOption ==> Some(Nested(Left(0)))
    }
  }

  group("flag .enableCollectionFallbackMerge(collectionFallbackMergeStrategy)") {

    test("should merge sequential-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      List("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(ListSet("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[ListSet[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(ListSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      ListSet("1", "2", "3", "4", "5")
        .intoPartial[List[Int]]
        .withFallback(Vector("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

      Nested(List("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(ListSet("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[ListSet[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(ListSet(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
      Nested(ListSet("1", "2", "3", "4", "5"))
        .intoPartial[Nested[List[Int]]]
        .withFallback(Nested(Vector("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    }

    test("should merge Map-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      ListMap("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))

      Nested(Map("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[Map[Int, Int]]]
        .withFallback(Nested(Map("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
    }

    test("should merge sequential-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      List("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(ListSet("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Vector(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[ListSet[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(ListSet(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))
      ListSet("1", "2", "3", "4", "5")
        .intoPartial[List[Int]]
        .withFallback(Vector("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(List(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))

      Nested(List("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(ListSet("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(Vector(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[ListSet[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(ListSet(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
      Nested(ListSet("1", "2", "3", "4", "5"))
        .intoPartial[Nested[List[Int]]]
        .withFallback(Nested(Vector("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(List(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
    }

    test("should merge Map-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      ListMap("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))

      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))
    }
  }

  group("flag .disableCollectionFallbackMerge") {

    test("should disable globally enabled .enableCollectionFallbackMerge") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      implicit val cfg = TransformerConfiguration.default.enableCollectionFallbackMerge(SourceAppendFallback)

      List("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(ListSet("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[ListSet[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(ListSet(1, 2, 3, 4, 5))
      ListSet("1", "2", "3", "4", "5")
        .intoPartial[List[Int]]
        .withFallback(Vector("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(List(1, 2, 3, 4, 5))

      Nested(List("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(ListSet("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[ListSet[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(ListSet(1, 2, 3, 4, 5)))
      Nested(ListSet("1", "2", "3", "4", "5"))
        .intoPartial[Nested[List[Int]]]
        .withFallback(Nested(Vector("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(List(1, 2, 3, 4, 5)))

      ListMap("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
    }
  }
}
