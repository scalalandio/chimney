package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.enginetestextension.{TestCollection, TestDict, TestPossible, TestWrapper}
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

/** Proves that Hearth `StandardMacroExtension`s (ServiceLoader-registered via `META-INF/services`, here the test-only
  * `chimney-engine-test-extension` module) are consulted by the derivation engine as a BUILT-IN FALLBACK layer:
  *   - `IsValueType` plugs into the ValueClassToType/TypeToValueClass rules (via `WrapperClassType`),
  *   - `IsCollection`/`IsMap` plug into the IterableToIterable/MapToMap rules (via `TotallyBuildIterable`),
  *   - `IsOption` plugs into the OptionToOption/ToOption/PartialOptionToNonOption rules (via `OptionalValue`),
  * and that the documented precedence holds: user implicits (rule #1) and `io.scalaland.chimney.integrations`
  * implicits both OVERRIDE extension-provided support.
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

    test("extension-provided value type stays gated behind the nonAnyValWrappers flag (like every non-AnyVal wrapper)") {
      compileErrors(""""abc".transformInto[TestWrapper]""").arePresent()
      compileErrors("""TestWrapper.wrap("abc").transformInto[String]""").arePresent()
    }

    test("user-provided implicit Transformer overrides the extension-provided value type support") {
      // The flag comes from the implicit config, NOT from the DSL: local DSL overrides suspend the Implicit rule
      // (by design), so the precedence claim is only provable with an override-free configuration.
      @scala.annotation.unused implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers
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
}
object HearthStdExtensionsIntegrationsSpec {

  case class Plain(value: String)
  case class PlainCopy(value: String)
  case class Wrapped(value: TestWrapper)

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
