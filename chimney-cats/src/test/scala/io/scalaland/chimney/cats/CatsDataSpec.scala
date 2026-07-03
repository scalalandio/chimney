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
  *   - NonEmpty* types are detected as SMART-CONSTRUCTOR collections: `PartialTransformer` derivation works, TOTAL
  *     derivation fails to compile (1.x was the same for stdlib->NonEmpty; what is NEW is that NonEmpty->NonEmpty and
  *     any `F[A] -> F[B]` mapping via `Traverse`/`~>` is no longer total - those `TotalOuterTransformer` instances were
  *     removed without a kindlings counterpart),
  *   - empty-input error MESSAGE drift: 1.x `PartiallyBuildIterable` implicits produced `"" -> "empty value"`
  *     (`partial.Result.fromOption`), kindlings providers produce `"" -> "Cannot create <Type> from empty collection"`.
  *     Paths are unchanged (error at the collection itself; element errors still at "(idx)"/"keys(k)"),
  *   - `NonEmptySeq`/`NonEmptyLazyList` have NO kindlings 0.3.0 provider - conversions are GONE (pinned below),
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

  test("BEHAVIOR GAP (2.0.0): Traverse-based TOTAL mapping between NonEmpty collections was removed (pinned)") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    // NonEmptyList still derives TOTALLY - not as a collection, but as a plain case class (head + tail), which
    // yields the same elementwise mapping the removed Traverse-based instance produced:
    NonEmptyList.one(1).transformInto[NonEmptyList[String]] ==> NonEmptyList.one("1")

    // The newtype-encoded (NonEmptyChain/NonEmptyMap/NonEmptySet) and private-constructor (NonEmptyVector) types
    // lost their TOTAL path: 1.x derived these via catsTotalOuterTransformerFromTraverse/-ForNonEmptyMap/
    // -ForNonEmptySet (TotalOuterTransformer instances, deleted without a kindlings counterpart) - use
    // transformIntoPartial (previous test) instead.
    compileErrors("""NonEmptyChain.one(1).transformInto[NonEmptyChain[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""NonEmptyVector.one(1).transformInto[NonEmptyVector[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""NonEmptyMap.one(1, 1).transformInto[NonEmptyMap[String, String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""NonEmptySet.one(1).transformInto[NonEmptySet[String]]""").check(
      "Chimney can't derive transformation from"
    )
  }

  test("BEHAVIOR GAP (2.0.0): FunctionK-based total transformation was removed (pinned)") {
    // 1.x derived F[A] -> G[B] TOTALLY given `F ~> G` + `Traverse[F]` (catsTotalTransformerFromFunctionK).
    // The instance was deleted and no extension mechanism replaces arbitrary-FunctionK support.
    compileErrors("""
      implicit val listToOption: _root_.cats.arrow.FunctionK[List, Option] =
        new _root_.cats.arrow.FunctionK[List, Option] {
          def apply[A](fa: List[A]): Option[A] = fa.headOption
        }
      List(1, 2, 3).transformInto[Option[Int]]
    """).check("Chimney can't derive transformation from")
  }

  test("BEHAVIOR GAP (2.0.0): NonEmptySeq and NonEmptyLazyList are unsupported (pinned)") {
    // kindlings-cats-integration 0.3.0 ships providers for NonEmptyList/NonEmptyVector/NonEmptyChain/Chain/
    // NonEmptySet/NonEmptyMap but NOT for NonEmptySeq/NonEmptyLazyList - report upstream; users can bring their
    // own io.scalaland.chimney.integrations.PartiallyBuildIterable in the meantime.
    compileErrors("""List("test").transformIntoPartial[_root_.cats.data.NonEmptySeq[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""_root_.cats.data.NonEmptySeq.one("test").transformInto[List[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""List("test").transformIntoPartial[_root_.cats.data.NonEmptyLazyList[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""_root_.cats.data.NonEmptyLazyList("test").transformInto[List[String]]""").check(
      "Chimney can't derive transformation from"
    )
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
