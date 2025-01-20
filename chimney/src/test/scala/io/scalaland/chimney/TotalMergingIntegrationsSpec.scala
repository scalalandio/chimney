package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.collection.immutable.ListMap

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

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .transform ==> Vector("1", "2", "3", "4", "5")
      Vector(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5")
      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5")

      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .transform ==> Nested(Vector("1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      ListMap(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")

      Nested(CustomCollection.of(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
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

    test("should use only source sequential-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallbackFrom(_.value)(CustomCollection.of(6, 7, 8, 9, 10))
        .transform ==> Nested(Vector("1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallbackFrom(_.value)(List(6, 7, 8, 9, 10))
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallbackFrom(_.value)(CustomCollection.of(6, 7, 8, 9, 10))
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))

      Nested(Nested(CustomCollection.of(1, 2, 3, 4, 5)))
        .into[Nested[Nested[Vector[String]]]]
        .withFallbackFrom(_.value)(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(Vector("1", "2", "3", "4", "5")))
      Nested(Nested(Vector(1, 2, 3, 4, 5)))
        .into[Nested[Nested[CustomCollection[String]]]]
        .withFallbackFrom(_.value)(Nested(List(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(CustomCollection.of("1", "2", "3", "4", "5")))
      Nested(Nested(CustomCollection.of(1, 2, 3, 4, 5)))
        .into[Nested[Nested[CustomCollection[String]]]]
        .withFallbackFrom(_.value)(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .transform ==> Nested(Nested(CustomCollection.of("1", "2", "3", "4", "5")))
    }

    test("should use only source Map-type when no merging strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallbackFrom(_.value)(CustomMap.of(5 -> 6, 7 -> 8))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallbackFrom(_.value)(ListMap(5 -> 6, 7 -> 8))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallbackFrom(_.value)(CustomMap.of(5 -> 6, 7 -> 8))
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")

      Nested(Nested(CustomMap.of(1 -> 2, 3 -> 4)))
        .into[Nested[Nested[ListMap[String, String]]]]
        .withFallbackFrom(_.value)(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .transform
        .value
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(Nested(ListMap(1 -> 2, 3 -> 4)))
        .into[Nested[Nested[CustomMap[String, String]]]]
        .withFallbackFrom(_.value)(Nested(ListMap(5 -> 6, 7 -> 8)))
        .transform
        .value
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(Nested(CustomMap.of(1 -> 2, 3 -> 4)))
        .into[Nested[Nested[CustomMap[String, String]]]]
        .withFallbackFrom(_.value)(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .transform
        .value
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
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

  group("flag .disableOptionFallbackMerge") {

    test("should disable globally enabled .enableOptionFallbackMerge") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      implicit val cfg = TransformerConfiguration.default.enableOptionFallbackMerge(SourceOrElseFallback)

      10
        .into[Possible[String]]
        .withFallback("fallback")
        .disableOptionFallbackMerge
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .disableOptionFallbackMerge
        .transform ==> Possible.Present("10")
      Possible(10)
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .disableOptionFallbackMerge
        .transform ==> Possible.Present("10")
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible("fallback"))
        .disableOptionFallbackMerge
        .transform ==> Possible.Nope
      (Possible.Nope: Possible[Int])
        .into[Possible[String]]
        .withFallback(Possible.Nope: Possible[String])
        .disableOptionFallbackMerge
        .transform ==> Possible.Nope

      Nested(10)
        .into[Nested[Possible[String]]]
        .withFallback(Nested("fallback"))
        .disableOptionFallbackMerge
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .disableOptionFallbackMerge
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible(10))
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .disableOptionFallbackMerge
        .transform ==> Nested(Possible.Present("10"))
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible("fallback")))
        .disableOptionFallbackMerge
        .transform ==> Nested(Possible.Nope)
      Nested(Possible.Nope: Possible[Int])
        .into[Nested[Possible[String]]]
        .withFallback(Nested(Possible.Nope: Possible[String]))
        .disableOptionFallbackMerge
        .transform ==> Nested(Possible.Nope)
    }
  }

  group("flag .enableCollectionFallbackMerge(collectionFallbackMergeStrategy)") {

    test("should merge sequential-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Vector("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      Vector(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(Vector("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    }

    test("should merge Map-types from source to fallback when SourceAppendFallback strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
      ListMap(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")

      Nested(CustomCollection.of(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4", "5" -> "6", "7" -> "8")
    }

    test("should merge sequential-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Vector("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")
      Vector(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> CustomCollection.of("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")
      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> CustomCollection.of("6", "7", "8", "9", "10", "1", "2", "3", "4", "5")

      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(Vector("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(CustomCollection.of("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform ==> Nested(CustomCollection.of("6", "7", "8", "9", "10", "1", "2", "3", "4", "5"))
    }

    test("should merge Map-types from fallback to source when FallbackAppendSource strategy is enabled") {
      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
      ListMap(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")

      Nested(CustomCollection.of(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .value
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .value
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
        .value
        .toVector ==> Vector("5" -> "6", "7" -> "8", "1" -> "2", "3" -> "4")
    }
  }

  group("flag .disableCollectionFallbackMerge") {

    test("should disable globally enabled .enableCollectionFallbackMerge") {

      import merges.Nested

      implicit val i2s: Transformer[Int, String] = _.toString

      implicit val cfg = TransformerConfiguration.default.enableCollectionFallbackMerge(SourceAppendFallback)

      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[Vector[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .disableCollectionFallbackMerge
        .transform ==> Vector("1", "2", "3", "4", "5")
      Vector(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(List(6, 7, 8, 9, 10))
        .disableCollectionFallbackMerge
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5")
      CustomCollection
        .of(1, 2, 3, 4, 5)
        .into[CustomCollection[String]]
        .withFallback(CustomCollection.of(6, 7, 8, 9, 10))
        .disableCollectionFallbackMerge
        .transform ==> CustomCollection.of("1", "2", "3", "4", "5")

      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[Vector[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .disableCollectionFallbackMerge
        .transform ==> Nested(Vector("1", "2", "3", "4", "5"))
      Nested(Vector(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(List(6, 7, 8, 9, 10)))
        .disableCollectionFallbackMerge
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))
      Nested(CustomCollection.of(1, 2, 3, 4, 5))
        .into[Nested[CustomCollection[String]]]
        .withFallback(Nested(CustomCollection.of(6, 7, 8, 9, 10)))
        .disableCollectionFallbackMerge
        .transform ==> Nested(CustomCollection.of("1", "2", "3", "4", "5"))

      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[ListMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .disableCollectionFallbackMerge
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      ListMap(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(ListMap(5 -> 6, 7 -> 8))
        .disableCollectionFallbackMerge
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      CustomMap
        .of(1 -> 2, 3 -> 4)
        .into[CustomMap[String, String]]
        .withFallback(CustomMap.of(5 -> 6, 7 -> 8))
        .disableCollectionFallbackMerge
        .transform
        .toVector ==> Vector("1" -> "2", "3" -> "4")

      Nested(CustomCollection.of(1 -> 2, 3 -> 4))
        .into[Nested[ListMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .disableCollectionFallbackMerge
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(ListMap(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(ListMap(5 -> 6, 7 -> 8)))
        .disableCollectionFallbackMerge
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
      Nested(CustomMap.of(1 -> 2, 3 -> 4))
        .into[Nested[CustomMap[String, String]]]
        .withFallback(Nested(CustomMap.of(5 -> 6, 7 -> 8)))
        .disableCollectionFallbackMerge
        .transform
        .value
        .toVector ==> Vector("1" -> "2", "3" -> "4")
    }
  }
}
