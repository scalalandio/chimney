package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.collection.immutable.ListMap

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

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5))
      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5))

      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5)))
      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5)))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      ListMap(1 -> 2, 3 -> 4)
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(CustomCollection.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
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

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallbackFrom(_.value)(CustomCollection.of("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[String]]]
        .withFallbackFrom(_.value)(List("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(CustomCollection.of("1", "2", "3", "4", "5")))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallbackFrom(_.value)(CustomCollection.of("6", "7", "8", "9", "10"))
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5)))

      Nested(Nested(CustomCollection.of("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[Vector[Int]]]]
        .withFallbackFrom(_.value)(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(Vector(1, 2, 3, 4, 5))))
      Nested(Nested(Vector("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[CustomCollection[Int]]]]
        .withFallbackFrom(_.value)(Nested(List("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(CustomCollection.of(1, 2, 3, 4, 5))))
      Nested(Nested(CustomCollection.of("1", "2", "3", "4", "5")))
        .intoPartial[Nested[Nested[CustomCollection[Int]]]]
        .withFallbackFrom(_.value)(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .transform
        .asOption ==> Some(Nested(Nested(CustomCollection.of(1, 2, 3, 4, 5))))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallbackFrom(_.value)(CustomMap.of("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallbackFrom(_.value)(ListMap("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallbackFrom(_.value)(CustomMap.of("5" -> "6", "7" -> "8"))
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(Nested(CustomMap.of("1" -> "2", "3" -> "4")))
        .intoPartial[Nested[Nested[ListMap[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(Nested(ListMap("1" -> "2", "3" -> "4")))
        .intoPartial[Nested[Nested[CustomMap[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(ListMap("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(Nested(CustomMap.of("1" -> "2", "3" -> "4")))
        .intoPartial[Nested[Nested[CustomMap[Int, Int]]]]
        .withFallbackFrom(_.value)(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
        .transform
        .asOption
        .map(_.value.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
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

  group("flag .disableOptionFallbackMerge") {

    test("should disable globally enabled .enableOptionFallbackMerge") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      implicit val cfg = TransformerConfiguration.default.enableOptionFallbackMerge(SourceOrElseFallback)

      "10"
        .intoPartial[Possible[Int]]
        .withFallback(20)
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Possible.Present(10))
      Possible("10")
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Possible.Present(10))
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Possible.Nope)
      (Possible.Nope: Possible[String])
        .intoPartial[Possible[Int]]
        .withFallback(Possible.Nope: Possible[Int])
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Possible.Nope)

      Nested("10")
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(20))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible("10"))
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Possible.Present(10)))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible(20)))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
      Nested(Possible.Nope: Possible[String])
        .intoPartial[Nested[Possible[Int]]]
        .withFallback(Nested(Possible.Nope: Possible[Int]))
        .disableOptionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Possible.Nope))
    }
  }

  group("flag .enableCollectionFallbackMerge(collectionFallbackMergeStrategy)") {

    test("should merge sequential-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    }

    test("should merge Map-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
      ListMap(1 -> 2, 3 -> 4)
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))

      Nested(CustomCollection.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8))
    }

    test("should merge sequential-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Vector(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(CustomCollection.of(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))
      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(CustomCollection.of(6, 7, 8, 9, 10, 1, 2, 3, 4, 5))

      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(Vector(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)))
    }

    test("should merge Map-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val s2i: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))
      ListMap(1 -> 2, 3 -> 4)
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))
      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))

      Nested(CustomCollection.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))
      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(5 -> 6, 7 -> 8, 1 -> 2, 3 -> 4))
      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
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

      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[Vector[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Vector(1, 2, 3, 4, 5))
      Vector("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(List("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5))
      CustomCollection
        .of("1", "2", "3", "4", "5")
        .intoPartial[CustomCollection[Int]]
        .withFallback(CustomCollection.of("6", "7", "8", "9", "10"))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(CustomCollection.of(1, 2, 3, 4, 5))

      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[Vector[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(Vector(1, 2, 3, 4, 5)))
      Nested(Vector("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(List("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5)))
      Nested(CustomCollection.of("1", "2", "3", "4", "5"))
        .intoPartial[Nested[CustomCollection[Int]]]
        .withFallback(Nested(CustomCollection.of("6", "7", "8", "9", "10")))
        .disableCollectionFallbackMerge
        .transform
        .asOption ==> Some(Nested(CustomCollection.of(1, 2, 3, 4, 5)))

      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[ListMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      ListMap(1 -> 2, 3 -> 4)
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(ListMap("5" -> "6", "7" -> "8"))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      CustomMap
        .of("1" -> "2", "3" -> "4")
        .intoPartial[CustomMap[Int, Int]]
        .withFallback(CustomMap.of("5" -> "6", "7" -> "8"))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))

      Nested(CustomCollection.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[ListMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(ListMap("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(ListMap("5" -> "6", "7" -> "8")))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
      Nested(CustomMap.of("1" -> "2", "3" -> "4"))
        .intoPartial[Nested[CustomMap[Int, Int]]]
        .withFallback(Nested(CustomMap.of("5" -> "6", "7" -> "8")))
        .disableCollectionFallbackMerge
        .transform
        .asOption
        .map(_.value.toVector) ==> Some(Vector(1 -> 2, 3 -> 4))
    }
  }
}
