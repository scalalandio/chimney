package io.scalaland.chimney.cats

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

/** Since 2.0.0 `chimney-cats` ships NO `io.scalaland.chimney.integrations.*` implicits for Cats collections
  * (`CatsDataImplicits` was removed). Instead, the derivation engine consults Hearth `StandardMacroExtension`
  * providers, and this suite runs with `com.kubuszok::kindlings-cats-integration` on its (test) compile classpath - NO
  * import is needed for the conversions below.
  *
  * BEHAVIOR CHANGES vs 1.x (documented per-test):
  *   - NonEmpty* types are detected as SMART-CONSTRUCTOR collections for stdlib->NonEmpty (`PartialTransformer` works,
  *     TOTAL fails to compile - same as 1.x). NonEmpty->NonEmpty and any `F[A] -> F[B]`/`F[A] -> G[B]` mapping via
  *     `Traverse`/`~>` stay TOTAL, RESTORED by `chimney-cats`' `CatsChimneyMacroExtension` (the 1.x
  *     `catsTotalOuterTransformerFromTraverse`/`-ForNonEmptyMap`/`-ForNonEmptySet`/`catsTotalTransformerFromFunctionK`
  *     instances, deleted with `CatsDataImplicits`, are re-provided as engine `SpecialCaseHandler`s),
  *   - empty-input error MESSAGE drift: 1.x `PartiallyBuildIterable` implicits produced `"" -> "empty value"`
  *     (`partial.Result.fromOption`), kindlings providers produce `"" -> "Cannot create <Type> from empty collection"`.
  *     Paths are unchanged (error at the collection itself; element errors still at "(idx)"/"keys(k)"),
  *   - `NonEmptySeq`/`NonEmptyLazyList` are supported again (kindlings providers since kubuszok/kindlings#163),
  *   - `NonEmptyMap`/`NonEmptySet` require `cats.Order` of the key/element to be summonable at MACRO-EXPANSION time
  *     (1.x required `Ordering` at implicit-summoning time - same effective requirement, different mechanism).
  *
  * Kindlings' Scala 3 artifacts are built with Scala 3.8.x (TASTy 28.8) - readable now that chimney builds with Scala
  * 3.8.4+, so this spec is SHARED between both Scala versions.
  */
class CatsDataSpec extends ChimneySpec {

  test("DSL should handle transformation to and from cats.data.Chain (total, via extension)") {
    List("test").transformInto[Chain[String]] ==> Chain("test")

    Chain("test").transformInto[List[String]] ==> List("test")

    // Chain's provider is total (plain constructor) - element mapping stays total, like 1.x's TotallyBuildIterable:
    implicit val intToStr: Transformer[Int, String] = _.toString
    Chain(1, 2).transformInto[Chain[String]] ==> Chain("1", "2")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyChain") {
    List("test").transformIntoPartial[NonEmptyChain[String]].asOption ==> Some(NonEmptyChain.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyChain[String]].asOption ==> None
    // MESSAGE DRIFT vs 1.x: was `"" -> "empty value"` (partial.Result.fromOption); path unchanged
    List.empty[String].transformIntoPartial[NonEmptyChain[String]].asErrorPathMessageStrings ==> Iterable(
      "" -> "Cannot create NonEmptyChain from empty collection"
    )

    NonEmptyChain.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyList") {
    List("test").transformIntoPartial[NonEmptyList[String]].asOption ==> Some(NonEmptyList.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyList[String]].asOption ==> None
    // MESSAGE DRIFT vs 1.x: was `"" -> "empty value"` (partial.Result.fromOption); path unchanged
    List.empty[String].transformIntoPartial[NonEmptyList[String]].asErrorPathMessageStrings ==> Iterable(
      "" -> "Cannot create NonEmptyList from empty collection"
    )

    // element-level failures keep their 1.x paths (error at the failing index, not at the collection)
    implicit val intParser: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)
    List("1", "x").transformIntoPartial[NonEmptyList[Int]].asErrorPathMessageStrings ==> Iterable(
      "(1)" -> "empty value"
    )

    NonEmptyList.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyMap") {
    List("test" -> "test").transformIntoPartial[NonEmptyMap[String, String]].asOption ==> Some(
      NonEmptyMap.one("test", "test")
    )
    List.empty[(String, String)].transformIntoPartial[NonEmptyMap[String, String]].asOption ==> None
    // MESSAGE DRIFT vs 1.x: was `"" -> "empty value"` (partial.Result.fromOption); path unchanged
    List.empty[(String, String)].transformIntoPartial[NonEmptyMap[String, String]].asErrorPathMessageStrings ==>
      Iterable("" -> "Cannot create NonEmptyMap from empty collection")

    NonEmptyMap.one("test", "test").transformInto[List[(String, String)]] ==> List("test" -> "test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptySet") {
    List("test").transformIntoPartial[NonEmptySet[String]].asOption ==> Some(NonEmptySet.one("test"))
    List.empty[String].transformIntoPartial[NonEmptySet[String]].asOption ==> None
    // MESSAGE DRIFT vs 1.x: was `"" -> "empty value"` (partial.Result.fromOption); path unchanged
    List.empty[String].transformIntoPartial[NonEmptySet[String]].asErrorPathMessageStrings ==> Iterable(
      "" -> "Cannot create NonEmptySet from empty collection"
    )

    NonEmptySet.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyVector") {
    List("test").transformIntoPartial[NonEmptyVector[String]].asOption ==> Some(NonEmptyVector.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyVector[String]].asOption ==> None
    // MESSAGE DRIFT vs 1.x: was `"" -> "empty value"` (partial.Result.fromOption); path unchanged
    List.empty[String].transformIntoPartial[NonEmptyVector[String]].asErrorPathMessageStrings ==> Iterable(
      "" -> "Cannot create NonEmptyVector from empty collection"
    )

    NonEmptyVector.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should allow PARTIAL transformation between NonEmpty collections (total is pinned as unsupported)") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    NonEmptyChain.one(1).transformIntoPartial[NonEmptyChain[String]].asOption ==> Some(NonEmptyChain.one("1"))
    NonEmptyList.one(1).transformIntoPartial[NonEmptyList[String]].asOption ==> Some(NonEmptyList.one("1"))
    NonEmptyMap.one(1, 1).transformIntoPartial[NonEmptyMap[String, String]].asOption ==> Some(
      NonEmptyMap.one("1", "1")
    )
    NonEmptyVector.one(1).transformIntoPartial[NonEmptyVector[String]].asOption ==> Some(NonEmptyVector.one("1"))
    NonEmptySet.one(1).transformIntoPartial[NonEmptySet[String]].asOption ==> Some(NonEmptySet.one("1"))
  }

  test("total transformation into NonEmpty collections fails compilation informatively (pinned)") {
    // Same as 1.x for stdlib sources: a smart-constructor (partial-only) target cannot be built totally.
    compileErrors("""List("test").transformInto[NonEmptyList[String]]""").check(
      "Chimney can't derive transformation from",
      "NonEmptyList"
    )
    compileErrors("""Map("a" -> 1).transformInto[NonEmptyMap[String, Int]]""").check(
      "Chimney can't derive transformation from",
      "NonEmptyMap"
    )
  }

  test("Traverse-based TOTAL mapping between NonEmpty collections (restored via CatsChimneyMacroExtension)") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    // RESTORED (2.0.0): 1.x's catsTotalOuterTransformerFromTraverse/-ForNonEmptyMap/-ForNonEmptySet (deleted with
    // CatsDataImplicits) are re-provided by chimney-cats' CatsChimneyMacroExtension - a Traverse[F].map handler for
    // NonEmptyChain/NonEmptyVector/NonEmptyList, plus dedicated NonEmptyMap/NonEmptySet handlers (those two have no
    // cats.Traverse, so they map via mapBoth/map given a summonable cats.Order of the target key/element).
    NonEmptyList.one(1).transformInto[NonEmptyList[String]] ==> NonEmptyList.one("1")
    NonEmptyChain.one(1).transformInto[NonEmptyChain[String]] ==> NonEmptyChain.one("1")
    NonEmptyVector.one(1).transformInto[NonEmptyVector[String]] ==> NonEmptyVector.one("1")
    NonEmptyMap.one(1, 1).transformInto[NonEmptyMap[String, String]] ==> NonEmptyMap.one("1", "1")
    NonEmptySet.one(1).transformInto[NonEmptySet[String]] ==> NonEmptySet.one("1")
  }

  test("FunctionK-based total transformation F[A] -> G[B] (restored via CatsChimneyMacroExtension)") {
    // RESTORED (2.0.0): 1.x's catsTotalTransformerFromFunctionK (F[A] -> G[B] given `F ~> G` + `Traverse[F]`) is
    // re-provided by CatsChimneyMacroExtension - it summons both cats.Traverse[F] and cats.arrow.FunctionK[F, G] at
    // macro-expansion time and produces `fk(Traverse[F].map(src)(inner))`.
    implicit val listToOption: _root_.cats.arrow.FunctionK[List, Option] =
      new _root_.cats.arrow.FunctionK[List, Option] {
        def apply[A](fa: List[A]): Option[A] = fa.headOption
      }
    List(1, 2, 3).transformInto[Option[Int]] ==> Some(1)
  }

  test("NonEmptySeq and NonEmptyLazyList (restored via kindlings-cats-integration providers)") {
    // RESTORED (2.0.0): kindlings-cats-integration now (kubuszok/kindlings#163) ships IsCollection providers for
    // NonEmptySeq/NonEmptyLazyList too, so these derive as smart-constructor collections WITHOUT any extension of our
    // own (just the dep on the classpath). Empty-input message follows kindlings' wording (see other tests).
    List("test").transformIntoPartial[_root_.cats.data.NonEmptySeq[String]].asOption ==> Some(
      _root_.cats.data.NonEmptySeq.one("test")
    )
    _root_.cats.data.NonEmptySeq.one("test").transformInto[List[String]] ==> List("test")
    List("test").transformIntoPartial[_root_.cats.data.NonEmptyLazyList[String]].asOption ==> Some(
      _root_.cats.data.NonEmptyLazyList("test")
    )
    _root_.cats.data.NonEmptyLazyList("test").transformInto[List[String]] ==> List("test")
  }

  test("Validated derivation stays unsupported: chimney's engine has no IsEither extension hook (pinned)") {
    // kindlings-cats-integration ships IsEitherProviderForValidated, but chimney's EitherToEither rule is
    // hardcoded to scala.Either and never consults Hearth IsEither providers - so Either <-> Validated does
    // not derive (it did not in 1.x either; `.asResult`/`.asValidated*` syntax remains the supported bridge).
    compileErrors(
      """(Right(1): Either[String, Int]).transformInto[_root_.cats.data.Validated[String, Int]]"""
    ).check("Chimney can't derive transformation from")
  }
}
