package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.enginetestextension.{
  TestCollection,
  TestDict,
  TestNonEmptyCollection,
  TestNonEmptyDict,
  TestPossible,
  TestSmartWrapper,
  TestSmartWrapperMulti,
  TestSmartWrapperThrowable,
  TestSmartWrapperThrowables,
  TestWrapper
}
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

/** Proves that Hearth `StandardMacroExtension`s (ServiceLoader-registered via `META-INF/services`, here the test-only
  * `chimney-engine-test-extension` module) are consulted by the derivation engine as a BUILT-IN FALLBACK layer:
  *   - `IsValueType` plugs into the ValueClassToType/TypeToValueClass rules (via `WrapperClassType`),
  *   - `IsCollection`/`IsMap` plug into the IterableToIterable/MapToMap rules (via `TotallyBuildIterable`),
  *   - `IsOption` plugs into the OptionToOption/ToOption/PartialOptionToNonOption rules (via `OptionalValue`),
  * and that the documented precedence holds: user implicits (rule #1) and `io.scalaland.chimney.integrations` implicits
  * both OVERRIDE extension-provided support.
  */
class HearthStdExtensionsIntegrationsSpec extends ChimneySpec {

  import HearthStdExtensionsIntegrationsSpec.*

  group("IsValueType extension providers (WrapperClassType fallback)") {

    test("transform into and from extension-provided value type, given the nonAnyValWrappers flag (total)") {
      "abc".into[TestWrapper].enableNonAnyValWrappers.transform ==> TestWrapper.wrap("abc")
      TestWrapper.wrap("abc").into[String].enableNonAnyValWrappers.transform ==> "abc"

      Plain("abc").into[Wrapped].enableNonAnyValWrappers.transform ==> Wrapped(TestWrapper.wrap("abc"))
      Wrapped(TestWrapper.wrap("abc")).into[Plain].enableNonAnyValWrappers.transform ==> Plain("abc")
    }

    test("transform into and from extension-provided value type, given the nonAnyValWrappers flag (partial)") {
      "abc".intoPartial[TestWrapper].enableNonAnyValWrappers.transform.asOption ==> Some(TestWrapper.wrap("abc"))
      TestWrapper.wrap("abc").intoPartial[String].enableNonAnyValWrappers.transform.asOption ==> Some("abc")

      Plain("abc").intoPartial[Wrapped].enableNonAnyValWrappers.transform.asOption ==> Some(
        Wrapped(TestWrapper.wrap("abc"))
      )
    }

    test(
      "extension-provided value type stays gated behind the nonAnyValWrappers flag (like every non-AnyVal wrapper)"
    ) {
      compileErrors(""""abc".transformInto[TestWrapper]""").arePresent()
      compileErrors("""TestWrapper.wrap("abc").transformInto[String]""").arePresent()
    }

    test("user-provided implicit Transformer overrides the extension-provided value type support") {
      // The flag comes from the implicit config, NOT from the DSL: local DSL overrides suspend the Implicit rule
      // (by design), so the precedence claim is only provable with an override-free configuration.
      @scala.annotation.unused
      implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers
      implicit val marker: Transformer[String, TestWrapper] = (src: String) => TestWrapper.wrap(src + "!custom")

      "abc".transformInto[TestWrapper] ==> TestWrapper.wrap("abc!custom")
    }
  }

  group("IsCollection extension providers (TotallyBuildIterable fallback)") {

    test("transform between built-in collections and extension-provided collection (total)") {
      List(1, 2, 3).transformInto[TestCollection[Int]] ==> TestCollection.of(1, 2, 3)
      TestCollection.of(1, 2, 3).transformInto[List[Int]] ==> List(1, 2, 3)
      TestCollection.of(1, 2, 3).transformInto[Vector[Int]] ==> Vector(1, 2, 3)
      TestCollection.of(1, 2, 3).transformInto[TestCollection[Int]] ==> TestCollection.of(1, 2, 3)
      // with element transformation
      TestCollection.of(Plain("a"), Plain("b")).transformInto[List[PlainCopy]] ==> List(PlainCopy("a"), PlainCopy("b"))
    }

    test("transform between built-in collections and extension-provided collection (partial)") {
      implicit val intParserOpt: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)

      List("1", "2").transformIntoPartial[TestCollection[Int]].asOption ==> Some(TestCollection.of(1, 2))
      TestCollection.of("1", "2").transformIntoPartial[List[Int]].asOption ==> Some(List(1, 2))
      List("1", "x").transformIntoPartial[TestCollection[Int]].asOption ==> None
      TestCollection.of("1", "x").transformIntoPartial[List[Int]].asOption ==> None
      // failure is reported at the failing index
      TestCollection.of("1", "x").transformIntoPartial[List[Int]].asErrorPathMessageStrings ==> Iterable(
        "(1)" -> "empty value"
      )
    }

    test("user-provided implicit Transformer overrides the extension-provided collection support") {
      implicit val reversing: Transformer[List[Int], TestCollection[Int]] =
        (src: List[Int]) => TestCollection.fromVector(src.reverse.toVector)

      List(1, 2, 3).transformInto[TestCollection[Int]] ==> TestCollection.of(3, 2, 1)
    }

    test("integrations-implicit TotallyBuildIterable overrides the extension-provided collection support") {
      import ReversingSupport.reversingTestCollectionFactory

      List(1, 2, 3).transformInto[TestCollection[Int]] ==> TestCollection.of(3, 2, 1)
      // NOTE: TestCollection[Int] -> TestCollection[Int] would short-circuit through the Subtypes rule (same-type
      // upcast, like List -> List today), so element types must differ for the iterable rule to be exercised.
      TestCollection.of(Plain("a"), Plain("b")).transformInto[TestCollection[PlainCopy]] ==> TestCollection.of(
        PlainCopy("b"),
        PlainCopy("a")
      )
    }
  }

  group("IsMap extension providers (TotallyBuildIterable fallback, pair-to-tuple adaptation)") {

    test("transform between scala.collection.Map and extension-provided map (total)") {
      Map("a" -> 1, "b" -> 2).transformInto[TestDict[String, Int]] ==> TestDict.of("a" -> 1, "b" -> 2)
      TestDict.of("a" -> 1, "b" -> 2).transformInto[Map[String, Int]] ==> Map("a" -> 1, "b" -> 2)
      TestDict.of("a" -> 1).transformInto[TestDict[String, Int]] ==> TestDict.of("a" -> 1)
      // extension-provided maps are also collections of pairs
      TestDict.of("a" -> 1).transformInto[List[(String, Int)]] ==> List("a" -> 1)
      // with key/value transformation
      TestDict.of(Plain("a") -> Plain("b")).transformInto[Map[PlainCopy, PlainCopy]] ==> Map(
        PlainCopy("a") -> PlainCopy("b")
      )
    }

    test("transform between scala.collection.Map and extension-provided map (partial)") {
      implicit val intParserOpt: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)

      Map("a" -> "1").transformIntoPartial[TestDict[String, Int]].asOption ==> Some(TestDict.of("a" -> 1))
      TestDict.of("a" -> "1").transformIntoPartial[Map[String, Int]].asOption ==> Some(Map("a" -> 1))
      Map("a" -> "x").transformIntoPartial[TestDict[String, Int]].asOption ==> None
      TestDict.of("a" -> "x").transformIntoPartial[Map[String, Int]].asOption ==> None
      // failure is reported at the failing key
      TestDict.of("a" -> "x").transformIntoPartial[Map[String, Int]].asErrorPathMessageStrings ==> Iterable(
        "(a)" -> "empty value"
      )
    }
  }

  group("IsOption extension providers (OptionalValue fallback)") {

    test("transform between Option and extension-provided optional (total)") {
      Option("a").transformInto[TestPossible[String]] ==> TestPossible.present("a")
      (None: Option[String]).transformInto[TestPossible[String]] ==> TestPossible.absent[String]
      TestPossible.present("a").transformInto[Option[String]] ==> Some("a")
      TestPossible.absent[String].transformInto[Option[String]] ==> None
      TestPossible.present(Plain("a")).transformInto[TestPossible[PlainCopy]] ==> TestPossible.present(PlainCopy("a"))
      // ToOption rule: non-optional into extension-provided optional
      "abc".transformInto[TestPossible[String]] ==> TestPossible.present("abc")
    }

    test("transform from extension-provided optional into non-optional (partial)") {
      TestPossible.present("a").transformIntoPartial[String].asOption ==> Some("a")
      TestPossible.absent[String].transformIntoPartial[String].asOption ==> None
      TestPossible.absent[String].transformIntoPartial[String].asErrorPathMessageStrings ==> Iterable(
        "" -> "empty value"
      )
    }

    test("user-provided implicit Transformer overrides the extension-provided optional support") {
      implicit val marker: Transformer[Option[String], TestPossible[String]] =
        (src: Option[String]) => TestPossible.fromOption(src.map(_ + "!custom"))

      Option("a").transformInto[TestPossible[String]] ==> TestPossible.present("a!custom")
    }
  }

  group("smart-constructor IsCollection extension providers (PartiallyBuildIterable fallback)") {

    test("transform into extension-provided non-empty collection (partial)") {
      List(1, 2, 3).transformIntoPartial[TestNonEmptyCollection[Int]].asOption ==> Some(
        TestNonEmptyCollection.of(1, 2, 3)
      )
      Vector("a").transformIntoPartial[TestNonEmptyCollection[String]].asOption ==> Some(
        TestNonEmptyCollection.of("a")
      )

      // with element transformation - failure is reported at the failing index
      implicit val intParserOpt: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)
      List("1", "2").transformIntoPartial[TestNonEmptyCollection[Int]].asOption ==> Some(
        TestNonEmptyCollection.of(1, 2)
      )
      List("1", "x").transformIntoPartial[TestNonEmptyCollection[Int]].asErrorPathMessageStrings ==> Iterable(
        "(1)" -> "empty value"
      )
    }

    test("failed smart constructor maps onto partial.Result like an integrations implicit would") {
      List.empty[Int].transformIntoPartial[TestNonEmptyCollection[Int]].asOption ==> None
      List.empty[Int].transformIntoPartial[TestNonEmptyCollection[Int]].asErrorPathMessageStrings ==> Iterable(
        "" -> "Cannot create TestNonEmptyCollection from empty collection"
      )
    }

    test("transform from extension-provided non-empty collection (total + partial)") {
      TestNonEmptyCollection.of(1, 2, 3).transformInto[List[Int]] ==> List(1, 2, 3)
      TestNonEmptyCollection.of(1, 2, 3).transformIntoPartial[Vector[Int]].asOption ==> Some(Vector(1, 2, 3))
    }

    test("Total transformer into smart-constructor collection fails compilation informatively") {
      compileErrors("""List(1, 2, 3).transformInto[TestNonEmptyCollection[Int]]""").check(
        "Chimney can't derive transformation from",
        "TestNonEmptyCollection"
      )
    }
  }

  group("smart-constructor IsMap extension providers (PartiallyBuildIterable fallback, pair-to-tuple adaptation)") {

    test("transform into extension-provided non-empty map (partial)") {
      Map("a" -> 1, "b" -> 2).transformIntoPartial[TestNonEmptyDict[String, Int]].asOption ==> Some(
        TestNonEmptyDict.of("a" -> 1, "b" -> 2)
      )

      // with value transformation - failure is reported at the failing key
      implicit val intParserOpt: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)
      Map("a" -> "1").transformIntoPartial[TestNonEmptyDict[String, Int]].asOption ==> Some(
        TestNonEmptyDict.of("a" -> 1)
      )
      Map("a" -> "x").transformIntoPartial[TestNonEmptyDict[String, Int]].asErrorPathMessageStrings ==> Iterable(
        "(a)" -> "empty value"
      )
    }

    test("failed smart constructor maps onto partial.Result like an integrations implicit would") {
      Map.empty[String, Int].transformIntoPartial[TestNonEmptyDict[String, Int]].asOption ==> None
      Map.empty[String, Int].transformIntoPartial[TestNonEmptyDict[String, Int]].asErrorPathMessageStrings ==> Iterable(
        "" -> "Cannot create TestNonEmptyDict from empty collection"
      )
    }

    test("transform from extension-provided non-empty map (total)") {
      TestNonEmptyDict.of("a" -> 1).transformInto[Map[String, Int]] ==> Map("a" -> 1)
      // extension-provided maps are also collections of pairs
      TestNonEmptyDict.of("a" -> 1).transformInto[List[(String, Int)]] ==> List("a" -> 1)
    }

    test("Total transformer into smart-constructor map fails compilation informatively") {
      compileErrors("""Map("a" -> 1).transformInto[TestNonEmptyDict[String, Int]]""").check(
        "Chimney can't derive transformation from",
        "TestNonEmptyDict"
      )
    }
  }

  group("smart-constructor IsValueType extension providers (PartialWrapperClassType fallback)") {

    test("wrap into smart-constructor value type (partial), given the nonAnyValWrappers flag") {
      "abc".intoPartial[TestSmartWrapper].enableNonAnyValWrappers.transform.asOption ==> Some(
        TestSmartWrapper.unsafe("abc")
      )
      "".intoPartial[TestSmartWrapper].enableNonAnyValWrappers.transform.asOption ==> None
      "".intoPartial[TestSmartWrapper].enableNonAnyValWrappers.transform.asErrorPathMessageStrings ==> Iterable(
        "" -> "TestSmartWrapper cannot be empty"
      )
    }

    test("every smart-constructor error shape maps onto partial.Result") {
      // EitherIterableStringOrValue
      "ab".intoPartial[TestSmartWrapperMulti].enableNonAnyValWrappers.transform.asOption ==> Some(
        TestSmartWrapperMulti.unsafe("ab")
      )
      "1".intoPartial[TestSmartWrapperMulti].enableNonAnyValWrappers.transform.asErrorPathMessageStrings ==> Iterable(
        "" -> "TestSmartWrapperMulti is too short",
        "" -> "TestSmartWrapperMulti contains digits"
      )
      // EitherThrowableOrValue
      "abc".intoPartial[TestSmartWrapperThrowable].enableNonAnyValWrappers.transform.asOption ==> Some(
        TestSmartWrapperThrowable.unsafe("abc")
      )
      "".intoPartial[TestSmartWrapperThrowable]
        .enableNonAnyValWrappers
        .transform
        .asErrorPathMessageStrings ==> Iterable(
        "" -> "TestSmartWrapperThrowable cannot be empty"
      )
      // EitherIterableThrowableOrValue
      "ab".intoPartial[TestSmartWrapperThrowables].enableNonAnyValWrappers.transform.asOption ==> Some(
        TestSmartWrapperThrowables.unsafe("ab")
      )
      "1"
        .intoPartial[TestSmartWrapperThrowables]
        .enableNonAnyValWrappers
        .transform
        .asErrorPathMessageStrings ==> Iterable(
        "" -> "TestSmartWrapperThrowables is too short",
        "" -> "TestSmartWrapperThrowables contains digits"
      )
    }

    test("smart-constructor value type inside a product reports errors at the field's path") {
      // NOTE: two-field products - a single-field case class would itself parse as a WrapperClass under the
      // nonAnyValWrappers flag and short-circuit through TypeToValueClass (with a path-less error at the value
      // itself), never reaching ProductToProduct's per-field path prepending.
      @scala.annotation.unused
      implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

      Plain2("abc", 1).transformIntoPartial[SmartWrapped2].asOption ==> Some(
        SmartWrapped2(TestSmartWrapper.unsafe("abc"), 1)
      )
      Plain2("", 1).transformIntoPartial[SmartWrapped2].asErrorPathMessageStrings ==> Iterable(
        "value" -> "TestSmartWrapper cannot be empty"
      )
    }

    test("unwrap smart-constructor value type as a source (total + partial)") {
      TestSmartWrapper.unsafe("abc").into[String].enableNonAnyValWrappers.transform ==> "abc"
      TestSmartWrapper.unsafe("abc").intoPartial[String].enableNonAnyValWrappers.transform.asOption ==> Some("abc")
    }

    test("Total transformer into smart-constructor value type fails compilation informatively") {
      compileErrors(""""abc".into[TestSmartWrapper].enableNonAnyValWrappers.transform""").check(
        "Chimney can't derive transformation from",
        "TestSmartWrapper"
      )
    }

    test("smart-constructor value type stays gated behind the nonAnyValWrappers flag") {
      compileErrors(""""abc".transformIntoPartial[TestSmartWrapper]""").arePresent()
    }
  }
}
object HearthStdExtensionsIntegrationsSpec {

  case class Plain(value: String)
  case class PlainCopy(value: String)
  case class Wrapped(value: TestWrapper)
  case class Plain2(value: String, another: Int)
  case class SmartWrapped2(value: TestSmartWrapper, another: Int)

  /** Scoped to single tests - ambient it would override the extension support everywhere in this spec. */
  object ReversingSupport {
    import io.scalaland.chimney.integrations.*

    import scala.collection.mutable

    implicit def reversingTestCollectionFactory[Item]: TotallyBuildIterable[TestCollection[Item], Item] =
      new TotallyBuildIterable[TestCollection[Item], Item] {

        def totalFactory: scala.collection.Factory[Item, TestCollection[Item]] =
          new FactoryCompat[Item, TestCollection[Item]] {
            override def newBuilder: mutable.Builder[Item, TestCollection[Item]] =
              new FactoryCompat.Builder[Item, TestCollection[Item]] {
                private val implBuilder = Vector.newBuilder[Item]

                override def clear(): Unit = implBuilder.clear()

                override def result(): TestCollection[Item] =
                  TestCollection.fromVector(implBuilder.result().reverse) // marker behavior

                override def addOne(elem: Item): this.type = { implBuilder += elem; this }
              }
          }

        override def iterator(collection: TestCollection[Item]): Iterator[Item] = collection.toVector.iterator
      }
  }
}
