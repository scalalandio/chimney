package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused

class PartialTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*
  import TotalTransformerStdLibTypesSpec.{Bar, Foo}

  test("transform using TotalOuterTransformer") {
    import OuterTransformers.totalNonEmptyToSorted

    val result = NonEmptyWrapper("b", "a").transformIntoPartial[SortedWrapper[String]]
    result.asOption ==> Some(SortedWrapper("a", "b"))
    result.asEither ==> Right(SortedWrapper("a", "b"))
    result.asErrorPathMessageStrings ==> Iterable.empty

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result2 = NonEmptyWrapper(Foo("b"), Foo("a")).transformIntoPartial[SortedWrapper[Bar]]
    result2.asOption ==> Some(SortedWrapper(Bar("a"), Bar("b")))
    result2.asEither ==> Right(SortedWrapper(Bar("a"), Bar("b")))
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using TotalOuterTransformer with an override") {
    import OuterTransformers.totalNonEmptyToSorted

    val result = NonEmptyWrapper("b", "a")
      .intoPartial[SortedWrapper[String]]
      .withFieldConst(_.everyItem, "c")
      .transform
    result.asOption ==> Some(SortedWrapper("c"))
    result.asEither ==> Right(SortedWrapper("c"))
    result.asErrorPathMessageStrings ==> Iterable.empty

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result2 = NonEmptyWrapper(Foo("b"), Foo("a"))
      .intoPartial[SortedWrapper[Bar]]
      .withFieldConst(_.everyItem.value, "c")
      .transform
    result2.asOption ==> Some(SortedWrapper(Bar("c")))
    result2.asEither ==> Right(SortedWrapper(Bar("c")))
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using PartialOuterTransformer") {
    import OuterTransformers.partialNonEmptyToSorted

    val result = NonEmptyWrapper("b", "a").transformIntoPartial[SortedWrapper[String]]
    result.asOption ==> Some(SortedWrapper("a", "b"))
    result.asEither ==> Right(SortedWrapper("a", "b"))
    result.asErrorPathMessageStrings ==> Iterable.empty

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result2 = NonEmptyWrapper(Foo("b"), Foo("a")).transformIntoPartial[SortedWrapper[Bar]]
    result2.asOption ==> Some(SortedWrapper(Bar("a"), Bar("b")))
    result2.asEither ==> Right(SortedWrapper(Bar("a"), Bar("b")))
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using PartialOuterTransformer with an override") {
    import OuterTransformers.partialNonEmptyToSorted

    val result = NonEmptyWrapper("b", "a")
      .intoPartial[SortedWrapper[String]]
      .withFieldConst(_.everyItem, "c")
      .transform
    result.asOption ==> Some(SortedWrapper("c"))
    result.asEither ==> Right(SortedWrapper("c"))
    result.asErrorPathMessageStrings ==> Iterable.empty

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result2 = NonEmptyWrapper(Foo("b"), Foo("a"))
      .intoPartial[SortedWrapper[Bar]]
      .withFieldConst(_.everyItem.value, "c")
      .transform
    result2.asOption ==> Some(SortedWrapper(Bar("c")))
    result2.asEither ==> Right(SortedWrapper(Bar("c")))
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using PartialOuterTransformer resolving total-partial-conflict") {
    import OuterTransformers.*

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result = NonEmptyWrapper(Foo("b"), Foo("a"))
      .intoPartial[SortedWrapper[Bar]]
      .enableImplicitConflictResolution(PreferTotalTransformer)
      .transform
    result.asOption ==> Some(SortedWrapper(Bar("a"), Bar("b")))
    result.asEither ==> Right(SortedWrapper(Bar("a"), Bar("b")))
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = NonEmptyWrapper(Foo("b"), Foo("a"))
      .intoPartial[SortedWrapper[Bar]]
      .enableImplicitConflictResolution(PreferPartialTransformer)
      .transform
    result2.asOption ==> Some(SortedWrapper(Bar("a"), Bar("b")))
    result2.asEither ==> Right(SortedWrapper(Bar("a"), Bar("b")))
    result2.asErrorPathMessageStrings ==> Iterable.empty

    compileErrors("""NonEmptyWrapper(Foo("b"), Foo("a")).transformIntoPartial[SortedWrapper[Bar]]""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyWrapper[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo] to io.scalaland.chimney.TotalTransformerIntegrationsSpec.SortedWrapper[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "io.scalaland.chimney.TotalTransformerIntegrationsSpec.SortedWrapper[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "  ambiguous implicits while resolving Chimney recursive transformation!",
      "TotalTransformerIntegrationsSpec.OuterTransformers.partialNonEmptyToSorted[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar](barOrdering)",
      "TotalTransformerIntegrationsSpec.OuterTransformers.totalNonEmptyToSorted[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar](barOrdering)",
      "  Please eliminate total/partial ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used.",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("transform using TotalOuterTransformer with an override") {
    import OuterTransformers.totalNonEmptyToSorted

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    val result = NonEmptyWrapper(Foo("b"), Foo("a"))
      .intoPartial[SortedWrapper[Bar]]
      .withFieldConst(_.everyItem.value, "c")
      .transform
    result.asOption ==> Some(SortedWrapper(Bar("c")))
    result.asEither ==> Right(SortedWrapper(Bar("c")))
    result.asErrorPathMessageStrings ==> Iterable.empty
  }

  group("transform from Option-type into Option-type, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-empty") {
      val result = Possible(123).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible("123"))
      result.asEither ==> Right(Possible("123"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is empty") {
      val result = (Possible.Nope: Possible[Int]).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from OptionalValue into OptionalValue, using Partial Transformer for inner type transformation") {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when Result is success") {
      val result = Possible("123").transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Present(123))
      result.asEither ==> Right(Possible.Present(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = Possible("abc").transformIntoPartial[Possible[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = (Possible.Nope: Possible[String]).transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from non-OptionalValue into OptionalValue, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-null") {
      val result = 10.transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Present("10"))
      result.asEither ==> Right(Possible.Present("10"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is null") {
      implicit val integerPrinter: Transformer[Integer, String] = _.toString

      val result = (null: Integer).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group(
    "transform from non-OptionalValue into OptionalValue, using Partial Transformer for inner type transformation"
  ) {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when Result is success") {
      val result = "123".transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Present(123))
      result.asEither ==> Right(Possible.Present(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = "abc".transformIntoPartial[Possible[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = (null: String).transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from OptionalValue into non-OptionalValue, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when option is non-empty") {
      val result = Possible(10).transformIntoPartial[String]

      result.asOption ==> Some("10")
      result.asEither ==> Right("10")
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is empty") {
      val result = Option.empty[Int].transformIntoPartial[String]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  group(
    "transform from OptionalValue into non-OptionalValue, using Partial Transformer for inner type transformation"
  ) {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when option is non-empty and inner is success") {
      val result = Possible("10").transformIntoPartial[Int]

      result.asOption ==> Some(10)
      result.asEither ==> Right(10)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is non-empty and inner is failure") {
      val result = Possible("abc").transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromErrorString("bad int"))
      result.asErrorPathMessageStrings ==> Iterable("" -> "bad int")
    }

    test("when option is empty") {
      val result = (Possible.Nope: Possible[String]).transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  test("transform into OptionalValue with an override") {
    "abc".intoPartial[Possible[String]].withFieldConst(_.matchingSome, "def").transform.asOption ==> Some(
      Possible("def")
    )
    Option("abc").intoPartial[Possible[String]].withFieldConst(_.matchingSome, "def").transform.asOption ==> Some(
      Possible("def")
    )
  }

  test(
    "transform TotallyBuildIterable/PartiallyBuildIterable to TotallyBuildIterable/PartiallyBuildIterable, using Total Transformer for inner type transformation"
  ) {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    CustomCollection
      .of(123, 456)
      .transformIntoPartial[Seq[String]]
      .asOption ==> Some(Seq("123", "456"))
    NonEmptyCollection
      .of(123, 456)
      .transformIntoPartial[Seq[String]]
      .asOption ==> Some(Seq("123", "456"))

    Seq(123, 456)
      .transformIntoPartial[CustomCollection[String]]
      .asOption ==> Some(CustomCollection.of("123", "456"))
    Seq(123, 456)
      .transformIntoPartial[NonEmptyCollection[String]]
      .asOption ==> Some(NonEmptyCollection.of("123", "456"))
    Seq
      .empty[Int]
      .transformIntoPartial[NonEmptyCollection[String]]
      .asOption ==> None

    CustomCollection
      .of(123, 456)
      .transformIntoPartial[CustomCollection[String]]
      .asOption ==> Some(CustomCollection.of("123", "456"))
    CustomCollection
      .of(123, 456)
      .transformIntoPartial[NonEmptyCollection[String]]
      .asOption ==> Some(NonEmptyCollection.of("123", "456"))
    CustomCollection
      .of[Int]()
      .transformIntoPartial[NonEmptyCollection[String]]
      .asOption ==> None

    NonEmptyCollection
      .of(123, 456)
      .transformIntoPartial[CustomCollection[String]]
      .asOption ==> Some(CustomCollection.of("123", "456"))
    NonEmptyCollection
      .of(123, 456)
      .transformIntoPartial[NonEmptyCollection[String]]
      .asOption ==> Some(NonEmptyCollection.of("123", "456"))
  }

  test(
    "transform TotallyBuildIterable/PartiallyBuildIterable to TotallyBuildIterable/PartiallyBuildIterable, using Partial Transformer for inner type transformation"
  ) {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    CustomCollection
      .of("123", "456")
      .transformIntoPartial[Seq[Int]]
      .asOption ==> Some(Seq(123, 456))
    NonEmptyCollection
      .of("123", "456")
      .transformIntoPartial[Seq[Int]]
      .asOption ==> Some(Seq(123, 456))

    Seq("123", "456")
      .transformIntoPartial[CustomCollection[Int]]
      .asOption ==> Some(CustomCollection.of(123, 456))
    Seq("123", "456")
      .transformIntoPartial[NonEmptyCollection[Int]]
      .asOption ==> Some(NonEmptyCollection.of(123, 456))
    Seq
      .empty[Int]
      .transformIntoPartial[NonEmptyCollection[Int]]
      .asOption ==> None

    CustomCollection
      .of("123", "456")
      .transformIntoPartial[CustomCollection[Int]]
      .asOption ==> Some(CustomCollection.of(123, 456))
    CustomCollection
      .of("123", "456")
      .transformIntoPartial[NonEmptyCollection[Int]]
      .asOption ==> Some(NonEmptyCollection.of(123, 456))
    CustomCollection
      .of[Int]()
      .transformIntoPartial[NonEmptyCollection[Int]]
      .asOption ==> None

    NonEmptyCollection
      .of("123", "456")
      .transformIntoPartial[CustomCollection[Int]]
      .asOption ==> Some(CustomCollection.of(123, 456))
    NonEmptyCollection
      .of("123", "456")
      .transformIntoPartial[NonEmptyCollection[Int]]
      .asOption ==> Some(NonEmptyCollection.of(123, 456))

    CustomCollection
      .of("abc", "123", "ghi")
      .transformIntoPartial[NonEmptyCollection[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value", "(2)" -> "empty value")
    NonEmptyCollection
      .of("abc", "123", "ghi")
      .transformIntoPartial[Seq[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value", "(2)" -> "empty value")
    Seq("abc", "123", "ghi")
      .transformIntoPartial[CustomCollection[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value", "(2)" -> "empty value")

    CustomCollection
      .of("abc", "123", "ghi")
      .transformIntoPartial[NonEmptyCollection[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value")
    NonEmptyCollection
      .of("abc", "123", "ghi")
      .transformIntoPartial[Seq[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value")
    Seq("abc", "123", "ghi")
      .transformIntoPartial[CustomCollection[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value")
  }

  test(
    "transform between Array-type and TotallyBuildIterable/PartiallyBuildIterable, using Total Transformer for inner type transformation"
  ) {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    Array(123, 456).transformIntoPartial[CustomCollection[String]].asOption ==> Some(CustomCollection.of("123", "456"))
    Array(123, 456).transformIntoPartial[NonEmptyCollection[String]].asOption ==> Some(
      NonEmptyCollection.of("123", "456")
    )

    CustomCollection.of(123, 456).transformIntoPartial[Array[String]].asOption.get ==> Array("123", "456")
    NonEmptyCollection.of(123, 456).transformIntoPartial[Array[String]].asOption.get ==> Array("123", "456")
  }

  test(
    "transform between Array-type and TotallyBuildIterable/PartiallyBuildIterable, using Partial Transformer for inner type transformation"
  ) {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Array("123", "456").transformIntoPartial[CustomCollection[Int]].asOption ==> Some(CustomCollection.of(123, 456))
    Array("123", "456").transformIntoPartial[NonEmptyCollection[Int]].asOption ==> Some(NonEmptyCollection.of(123, 456))

    Array("abc", "456").transformIntoPartial[CustomCollection[Int]].asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value"
    )
    Array("abc", "456").transformIntoPartial[NonEmptyCollection[Int]].asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value"
    )
    Array.empty[String].transformIntoPartial[CustomCollection[Int]].asOption ==> Some(CustomCollection.of[Int]())
    Array.empty[String].transformIntoPartial[NonEmptyCollection[Int]].asOption ==> None

    CustomCollection.of("123", "456").transformIntoPartial[Array[Int]].asOption.get ==> Array(123, 456)
    NonEmptyCollection.of("123", "456").transformIntoPartial[Array[Int]].asOption.get ==> Array(123, 456)
    CustomCollection.of("abc", "456").transformIntoPartial[Array[Int]].asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value"
    )
    NonEmptyCollection.of("abc", "456").transformIntoPartial[Array[Int]].asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value"
    )
  }

  test("transform into sequential type with an override") {
    Seq(Foo("a"))
      .intoPartial[CustomCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(CustomCollection.of(Bar("b")))
    CustomCollection
      .of(Foo("a"))
      .intoPartial[CustomCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(CustomCollection.of(Bar("b")))
    NonEmptyCollection
      .of(Foo("a"))
      .intoPartial[CustomCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(CustomCollection.of(Bar("b")))

    Seq(Foo("a"))
      .intoPartial[NonEmptyCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(NonEmptyCollection.of(Bar("b")))
    CustomCollection
      .of(Foo("a"))
      .intoPartial[NonEmptyCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(NonEmptyCollection.of(Bar("b")))
    NonEmptyCollection
      .of(Foo("a"))
      .intoPartial[NonEmptyCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform
      .asOption ==> Some(NonEmptyCollection.of(Bar("b")))
  }

  test(
    "transform TotallyBuildMap/PartiallyBuildMap to TotallyBuildMap/PartiallyBuildMap, using Total Transformer for inner type transformation"
  ) {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    CustomMap
      .of(123 -> 456)
      .transformIntoPartial[Map[String, String]]
      .asOption ==> Some(Map("123" -> "456"))
    NonEmptyMap
      .of(123 -> 456)
      .transformIntoPartial[Map[String, String]]
      .asOption ==> Some(Map("123" -> "456"))

    Map(123 -> 456)
      .transformIntoPartial[CustomMap[String, String]]
      .asOption ==> Some(CustomMap.of("123" -> "456"))
    Map(123 -> 456)
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> Some(NonEmptyMap.of("123" -> "456"))
    Map
      .empty[Int, Int]
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> None

    CustomMap
      .of(123 -> 456)
      .transformIntoPartial[CustomMap[String, String]]
      .asOption ==> Some(CustomMap.of("123" -> "456"))
    CustomMap
      .of(123 -> 456)
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> Some(NonEmptyMap.of("123" -> "456"))
    CustomMap
      .of[Int, Int]()
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> None

    NonEmptyMap
      .of(123 -> 456)
      .transformIntoPartial[CustomMap[String, String]]
      .asOption ==> Some(CustomMap.of("123" -> "456"))
    NonEmptyMap
      .of(123 -> 456)
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> Some(NonEmptyMap.of("123" -> "456"))
  }

  test(
    "transform TotallyBuildMap/PartiallyBuildMap to TotallyBuildMap/PartiallyBuildMap, using Partial Transformer for inner type transformation"
  ) {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    CustomMap
      .of("123" -> "456")
      .transformIntoPartial[Map[Int, Int]]
      .asOption ==> Some(Map(123 -> 456))
    NonEmptyMap
      .of("123" -> "456")
      .transformIntoPartial[Map[Int, Int]]
      .asOption ==> Some(Map(123 -> 456))

    Map("123" -> "456")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asOption ==> Some(CustomMap.of(123 -> 456))
    Map("123" -> "456")
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asOption ==> Some(NonEmptyMap.of(123 -> 456))
    Map
      .empty[Int, Int]
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asOption ==> None

    CustomMap
      .of("123" -> "456")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asOption ==> Some(CustomMap.of(123 -> 456))
    CustomMap
      .of("123" -> "456")
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asOption ==> Some(NonEmptyMap.of(123 -> 456))
    CustomMap
      .of[Int, Int]()
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asOption ==> None

    NonEmptyMap
      .of("123" -> "456")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asOption ==> Some(CustomMap.of(123 -> 456))
    NonEmptyMap
      .of("123" -> "456")
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asOption ==> Some(NonEmptyMap.of(123 -> 456))

    CustomMap
      .of("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[NonEmptyMap[Int, Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value", "(456)" -> "empty value")
    NonEmptyMap
      .of("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[Map[Int, Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value", "(456)" -> "empty value")
    Map("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[CustomMap[Int, Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value", "(456)" -> "empty value")

    CustomMap
      .of("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[NonEmptyMap[Int, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value")
    NonEmptyMap
      .of("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[Map[Int, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value")
    Map("abc" -> "123", "456" -> "ghi")
      .transformIntoPartial[CustomMap[Int, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value")
  }

  test(
    "transform between TotallyBuildIterable/PartiallyBuildIterable and TotallyBuildMap/PartiallyBuildMap, using Total Transformer for inner type transformation"
  ) {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    CustomCollection
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[CustomMap[Int, String]]
      .asOption ==> Some(CustomMap.of(1 -> "10", 2 -> "20"))
    NonEmptyCollection
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[CustomMap[String, String]]
      .asOption ==> Some(CustomMap.of("1" -> "10", "2" -> "20"))

    CustomCollection
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[NonEmptyMap[Int, String]]
      .asOption ==> Some(NonEmptyMap.of(1 -> "10", 2 -> "20"))
    NonEmptyCollection
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> Some(NonEmptyMap.of("1" -> "10", "2" -> "20"))

    CustomMap
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[CustomCollection[(Int, String)]]
      .asOption ==> Some(CustomCollection.of(1 -> "10", 2 -> "20"))
    NonEmptyMap
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[CustomCollection[(String, String)]]
      .asOption ==> Some(CustomCollection.of("1" -> "10", "2" -> "20"))

    CustomMap
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[NonEmptyCollection[(Int, String)]]
      .asOption ==> Some(NonEmptyCollection.of(1 -> "10", 2 -> "20"))
    NonEmptyMap
      .of(1 -> 10, 2 -> 20)
      .transformIntoPartial[NonEmptyCollection[(String, String)]]
      .asOption ==> Some(NonEmptyCollection.of("1" -> "10", "2" -> "20"))
  }

  test(
    "transform between TotallyBuildIterable/PartiallyBuildIterable and TotallyBuildMap/PartiallyBuildMap, using Partial Transformer for inner type transformation"
  ) {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    CustomCollection
      .of("1" -> "10", "2" -> "20")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asOption ==> Some(CustomMap.of(1 -> 10, 2 -> 20))
    NonEmptyCollection
      .of("1" -> "10", "2" -> "20")
      .transformIntoPartial[CustomMap[String, Int]]
      .asOption ==> Some(CustomMap.of("1" -> 10, "2" -> 20))
    CustomMap
      .of("1" -> "10", "2" -> "20")
      .transformIntoPartial[CustomCollection[(Int, Int)]]
      .asOption ==> Some(CustomCollection.of(1 -> 10, 2 -> 20))
    CustomMap
      .of("1" -> "10", "2" -> "20")
      .transformIntoPartial[NonEmptyCollection[(Int, String)]]
      .asOption ==> Some(NonEmptyCollection.of(1 -> "10", 2 -> "20"))

    CustomCollection
      .of("1" -> "10", "2" -> "x")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asErrorPathMessageStrings ==> Iterable("(1)._2" -> "empty value")
    NonEmptyCollection
      .of("1" -> "x", "2" -> "y")
      .transformIntoPartial[NonEmptyMap[String, Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)._2" -> "empty value", "(1)._2" -> "empty value")
    CustomMap
      .of("x" -> "10", "y" -> "z")
      .transformIntoPartial[CustomCollection[(Int, Int)]]
      .asErrorPathMessageStrings ==> Iterable(
      "keys(x)" -> "empty value",
      "keys(y)" -> "empty value",
      "(y)" -> "empty value"
    )
    NonEmptyMap
      .of("x" -> "10", "y" -> "z")
      .transformIntoPartial[NonEmptyCollection[(Int, Int)]]
      .asErrorPathMessageStrings ==> Iterable(
      "keys(x)" -> "empty value",
      "keys(y)" -> "empty value",
      "(y)" -> "empty value"
    )
  }

  test(
    "transform between Array-types and TotallyBuildMap/PartiallyBuildMap, using Total Transformer for inner type transformation"
  ) {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    Array(123 -> 456)
      .transformIntoPartial[CustomMap[String, String]]
      .asOption ==> Some(CustomMap.of("123" -> "456"))
    Array(123 -> 456)
      .transformIntoPartial[NonEmptyMap[String, String]]
      .asOption ==> Some(NonEmptyMap.of("123" -> "456"))

    CustomMap.of(123 -> 456).transformIntoPartial[Array[(String, String)]].asOption.get ==> Array("123" -> "456")
    NonEmptyMap.of(123 -> 456).transformIntoPartial[Array[(String, String)]].asOption.get ==> Array("123" -> "456")
  }

  test(
    "transform between Array-types and TotallyBuildMap/PartiallyBuildMap, using Partial Transformer for inner type transformation"
  ) {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Array("123" -> "456").transformIntoPartial[CustomMap[Int, Int]].asOption ==> Some(CustomMap.of(123 -> 456))
    Array("abc" -> "456")
      .transformIntoPartial[CustomMap[Int, Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)._1" -> "empty value")
    Array("123" -> "456").transformIntoPartial[NonEmptyMap[Int, Int]].asOption ==> Some(NonEmptyMap.of(123 -> 456))
    Array("abc" -> "456")
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)._1" -> "empty value")
    Array
      .empty[(String, String)]
      .transformIntoPartial[NonEmptyMap[Int, Int]]
      .asErrorPathMessageStrings ==> Iterable("" -> "empty value")

    CustomMap
      .of("abc" -> "456", "123" -> "ghi")
      .transformIntoPartial[Array[(Int, Int)]]
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value", "(123)" -> "empty value")
    NonEmptyMap
      .of("abc" -> "456", "123" -> "ghi")
      .transformIntoPartial[Array[(Int, Int)]]
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value", "(123)" -> "empty value")

    CustomMap
      .of("abc" -> "456", "123" -> "ghi")
      .transformIntoPartial[Array[(Int, Int)]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value")
    NonEmptyMap
      .of("abc" -> "456", "123" -> "ghi")
      .transformIntoPartial[Array[(Int, Int)]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(abc)" -> "empty value")
  }

  test("transform into map type with an override") {
    Map(Foo("k") -> Foo("v"))
      .intoPartial[CustomMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(CustomMap.of(Bar("k2") -> Bar("v2")))
    CustomMap
      .of(Foo("k") -> Foo("v"))
      .intoPartial[CustomMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(CustomMap.of(Bar("k2") -> Bar("v2")))
    NonEmptyMap
      .of(Foo("k") -> Foo("v"))
      .intoPartial[CustomMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(CustomMap.of(Bar("k2") -> Bar("v2")))

    Map(Foo("k") -> Foo("v"))
      .intoPartial[NonEmptyMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(NonEmptyMap.of(Bar("k2") -> Bar("v2")))
    CustomMap
      .of(Foo("k") -> Foo("v"))
      .intoPartial[NonEmptyMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(NonEmptyMap.of(Bar("k2") -> Bar("v2")))
    NonEmptyMap
      .of(Foo("k") -> Foo("v"))
      .intoPartial[NonEmptyMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform
      .asOption ==> Some(NonEmptyMap.of(Bar("k2") -> Bar("v2")))
  }

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrors("""Source("foo").transformIntoPartial[TargetWithOption]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "  y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""Source("foo").intoPartial[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "  y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo").intoPartial[TargetWithOption].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone
        Source("foo").transformIntoPartial[TargetWithOption].asOption ==> Some(TargetWithOption("foo", Possible.Nope))
      }
    }

    test(
      "should use OptionalValue.empty for fields without source but with default value when enabled but default values disabled"
    ) {
      Source("foo").intoPartial[TargetWithOptionAndDefault].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOptionAndDefault("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

        Source("foo").transformIntoPartial[TargetWithOptionAndDefault].asOption ==> Some(
          TargetWithOptionAndDefault("foo", Possible.Nope)
        )
      }
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .intoPartial[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOptionAndDefault(
          "foo",
          Possible.Present(42)
        )
      )
    }

    test(
      "use OptionalValue.empty for fields without source but with default value when enabled only for a single field when scoped using .withTargetFlag(_.field)"
    ) {
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .withTargetFlag(_.y)
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(TargetWithOptionAndDefault("foo", Possible.Nope))
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrors("""Source("foo").intoPartial[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "  y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enablePartialUnwrapsOption") {

    case class Source(a: Possible[String])
    case class Target(a: String)

    test("should be turned on by default") {
      Source(Possible.Present("value")).transformIntoPartial[Target].asOption ==> Some(Target("value"))
      Source(Possible.Present("value")).intoPartial[Target].transform.asOption ==> Some(Target("value"))
    }

    test("should re-enable globally disabled .disablePartialUnwrapsOption") {
      implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

      Source(Possible.Present("value")).intoPartial[Target].enablePartialUnwrapsOption.transform.asOption ==> Some(
        Target("value")
      )
    }

    test("should be turned on only for a single field when scoped using .withTargetFlag(_.field)") {
      implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

      Source(Possible.Present("value"))
        .intoPartial[Target]
        .withTargetFlag(_.a)
        .enablePartialUnwrapsOption
        .transform
        .asOption ==> Some(
        Target("value")
      )
    }
  }

  group("flag .disablePartialUnwrapsOption") {

    @unused case class Source(a: Possible[String])
    @unused case class Target(a: String)

    test("should fail compilation if OptionalValue unwrapping is not provided when disabled") {
      compileErrors(
        """Source(Possible.Present("value")).intoPartial[Target].disablePartialUnwrapsOption.transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
        "  a: java.lang.String - can't derive transformation from a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "java.lang.String (transforming from: a into: a)",
        "  derivation from source.a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      locally {
        @unused implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

        compileErrors("""Source(Possible.Present("value")).transformIntoPartial[Target]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
          "io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
          "  a: java.lang.String - can't derive transformation from a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
          "java.lang.String (transforming from: a into: a)",
          "  derivation from source.a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to java.lang.String is not supported in Chimney!",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }
  }
}
