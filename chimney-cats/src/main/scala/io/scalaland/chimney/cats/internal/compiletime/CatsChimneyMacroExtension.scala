package io.scalaland.chimney.cats.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.MIO
import io.scalaland.chimney.integrations.ChimneyMacroExtension
import io.scalaland.chimney.internal.compiletime.derivation.transformer.ChimneyEngineExtensionApi

/** Chimney `ChimneyMacroExtension` (registered via
  * `META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`, shipped in the published chimney-cats
  * jar) providing engine-aware, structural transformations for the Cats `Traverse`/`~>`/`NonEmptyMap`/`NonEmptySet`
  * conversions whose shape Hearth's std-extension providers (`IsCollection`/`IsMap`, in kindlings-cats-integration)
  * cannot express (a `TotalOuterTransformer`/`FunctionK`-style element mapping that PRESERVES the original container
  * rather than round-tripping through a builder).
  *
  * Since 2.0.0 these RESTORE the corresponding IMPLICITS deleted from `CatsDataImplicits`, so the conversions work
  * WITHOUT any import once this jar is on the classpath (user/integration implicits still override them - the
  * special-cased rule sits below the implicit rules):
  *
  *   - `catsTotalOuterTransformerFromTraverse`: ANY `F[A] -> F[B]` where `cats.Traverse[F]` is summonable (general
  *     handler #1, above the structural rules - it intercepts stdlib `List -> List` too, by design). Total via
  *     `Traverse[F].map`; partial via `Traverse[F].traverseWithIndexM[partial.Result, ...]` (index-aware error paths),
  *   - `catsTotalOuterTransformerForNonEmptyMap`: `NonEmptyMap[A, B] -> NonEmptyMap[C, D]` when `cats.kernel.Order[C]`
  *     is summonable (handler #2 - `NonEmptyMap` has NO `cats.Traverse`),
  *   - `catsTotalOuterTransformerForNonEmptySet`: `NonEmptySet[A] -> NonEmptySet[B]` when `cats.kernel.Order[B]` is
  *     summonable (handler #3 - `NonEmptySet` has NO `cats.Traverse`),
  *   - `catsTotalTransformerFromFunctionK`: `F[A] -> G[B]` when both `cats.Traverse[F]` and
  *     `cats.arrow.FunctionK[F, G]` (`F ~> G`) are summonable (handler #4, registered LAST so same-`F` pairs go to
  *     handler #1).
  *
  * The handlers are registered in that order; [[IsChimneySpecialCased]] takes the first that matches a `(From, To)`
  * pair (so `NonEmptySet`, which decomposes to one arg but has no `Traverse[NonEmptySet]`, falls through handler #1's
  * failed summon to handler #3).
  *
  * Cross-quotes notes: the phantom `AnyK1` constructor that a summoned `cats.Traverse[F]` / `F ~> G` is typed against
  * has no `scala.quoted.Type`, so it must never appear inside a quote. Two techniques keep it out (and keep Scala 2
  * reification happy): (1) every actual Cats operation lives in `internal.runtime.CatsMacroConversions` (normal Scala,
  * fully-qualified), so quotes only splice values into a helper call and `asInstanceOf`-cast the result - the summoned
  * instances are meta-level `Expr`-cast onto the erased `Traverse[AnyF]` / `FunctionK[Id, Id]` shapes first; (2) the
  * quote-building is done in helper methods with REGULAR `[A: Type]` type parameters (not `existential.Underlying`), so
  * element types reify through their `Type` evidence instead of a macro-local path. This module compiles with
  * `-Xsource:3`, so Scala-3 syntax (`.apply(using ...)`, `*` splices, `&`) is used.
  */
final class CatsChimneyMacroExtension extends ChimneyMacroExtension { loader =>

  override def extend(ctx: MacroCommons & ChimneyEngineExtensionApi): Unit = {
    import ctx.*

    val TraverseCtor = Type.CtorK1.of[_root_.cats.Traverse]
    val OrderCtor = Type.Ctor1.of[_root_.cats.kernel.Order]
    // `NonEmptyMap`/`NonEmptySet` are cats NEWTYPE aliases (`*Impl.Type`); round-tripping the constructor through
    // `fromUntyped(of[...].asUntyped)` normalizes it so `unapply` matches the dealiased applied form (as kindlings does).
    val NonEmptyMapCtor =
      Type.Ctor2.fromUntyped[_root_.cats.data.NonEmptyMap](Type.Ctor2.of[_root_.cats.data.NonEmptyMap].asUntyped)
    val NonEmptySetCtor =
      Type.Ctor1.fromUntyped[_root_.cats.data.NonEmptySet](Type.Ctor1.of[_root_.cats.data.NonEmptySet].asUntyped)
    // A concrete FunctionK application used only to obtain the `FunctionK` type constructor (there is no `CtorK2`).
    val FunctionKSample = Type.of[_root_.cats.arrow.FunctionK[List, Option]]

    // --- helper-method builders (REGULAR type params, so element types reify through `Type` evidence) ---

    // Erased `Traverse[AnyF]` and `FunctionK[Id, Id]` Exprs are what the runtime helpers accept (see CatsMacroConversions).
    type AnyF[x] = io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions.AnyF[x]

    def buildTraverse[From: Type, To: Type, A: Type, B: Type](
        traverse: Expr[_root_.cats.Traverse[AnyF]]
    ): SpecialCasedTransformation[From, To] =
      new SpecialCasedTransformation[From, To] {
        override def specialCase(implicit
            context: SpecialCaseContext[From, To]
        ): MIO[Option[DerivedExpr[To]]] =
          specialCasedOuterMap[From, To, A, B] { fn =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .mapTraverse(Expr.splice(traverse), Expr.splice(sourceOf(context)), Expr.splice(fn))
                .asInstanceOf[To]
            }
          } { (fn, failFast) =>
            val _ = failFast
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .traversePartial(Expr.splice(traverse), Expr.splice(sourceOf(context)), Expr.splice(fn))
                .asInstanceOf[io.scalaland.chimney.partial.Result[To]]
            }
          }
      }

    def buildFunctionK[From: Type, To: Type, A: Type, B: Type](
        traverse: Expr[_root_.cats.Traverse[AnyF]],
        functionK: Expr[_root_.cats.arrow.FunctionK[_root_.cats.Id, _root_.cats.Id]]
    ): SpecialCasedTransformation[From, To] =
      new SpecialCasedTransformation[From, To] {
        override def specialCase(implicit
            context: SpecialCaseContext[From, To]
        ): MIO[Option[DerivedExpr[To]]] =
          specialCasedOuterMap[From, To, A, B] { fn =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .mapTraverseThroughFunctionK(
                  Expr.splice(traverse),
                  Expr.splice(functionK),
                  Expr.splice(sourceOf(context)),
                  Expr.splice(fn)
                )
                .asInstanceOf[To]
            }
          } { (fn, failFast) =>
            val _ = failFast
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .traversePartialThroughFunctionK(
                  Expr.splice(traverse),
                  Expr.splice(functionK),
                  Expr.splice(sourceOf(context)),
                  Expr.splice(fn)
                )
                .asInstanceOf[io.scalaland.chimney.partial.Result[To]]
            }
          }
      }

    def buildNonEmptyMap[From: Type, To: Type, A: Type, B: Type, C: Type, D: Type](
        orderC: Expr[_root_.cats.kernel.Order[C]]
    ): SpecialCasedTransformation[From, To] = {
      implicit val PairFrom: Type[(A, B)] = Type.of[(A, B)]
      implicit val PairTo: Type[(C, D)] = Type.of[(C, D)]
      new SpecialCasedTransformation[From, To] {
        override def specialCase(implicit
            context: SpecialCaseContext[From, To]
        ): MIO[Option[DerivedExpr[To]]] =
          specialCasedOuterMap[From, To, (A, B), (C, D)] { fn =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .mapNonEmptyMap(Expr.splice(sourceOf(context)), Expr.splice(fn), Expr.splice(orderC))
                .asInstanceOf[To]
            }
          } { (fn, failFast) =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .traverseNonEmptyMap(
                  Expr.splice(sourceOf(context)),
                  Expr.splice(fn),
                  Expr.splice(failFast),
                  Expr.splice(orderC)
                )
                .asInstanceOf[io.scalaland.chimney.partial.Result[To]]
            }
          }
      }
    }

    def buildNonEmptySet[From: Type, To: Type, A: Type, B: Type](
        orderB: Expr[_root_.cats.kernel.Order[B]]
    ): SpecialCasedTransformation[From, To] =
      new SpecialCasedTransformation[From, To] {
        override def specialCase(implicit
            context: SpecialCaseContext[From, To]
        ): MIO[Option[DerivedExpr[To]]] =
          specialCasedOuterMap[From, To, A, B] { fn =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .mapNonEmptySet(Expr.splice(sourceOf(context)), Expr.splice(fn), Expr.splice(orderB))
                .asInstanceOf[To]
            }
          } { (fn, failFast) =>
            Expr.quote {
              io.scalaland.chimney.cats.internal.runtime.CatsMacroConversions
                .traverseNonEmptySet(
                  Expr.splice(sourceOf(context)),
                  Expr.splice(fn),
                  Expr.splice(failFast),
                  Expr.splice(orderB)
                )
                .asInstanceOf[io.scalaland.chimney.partial.Result[To]]
            }
          }
      }

    // --- Handler #1: general `F[A] -> F[B]` via `cats.Traverse[F]` (SAME `F`) ---
    registerSpecialCase(new SpecialCaseHandler {
      @scala.annotation.nowarn("msg=is never used")
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): Option[SpecialCasedTransformation[From, To]] =
        (Type.decompose1[From], Type.decompose1[To]) match {
          case (Some((fCtor, fArg)), Some((gCtor, toArg))) if fCtor.sameTypeConstructorAs(gCtor.asUntyped) =>
            import fArg.Underlying as A, toArg.Underlying as B
            implicit val TraverseFType: Type[_root_.cats.Traverse[AnyK1]] = TraverseCtor.apply(using fCtor)
            Expr.summonImplicit[_root_.cats.Traverse[AnyK1]].toOption.map { traverseExpr =>
              // Retype the summoned instance onto the erased, quote-safe `Traverse[AnyF]` (meta-level `Expr` cast).
              val traverse = traverseExpr.asInstanceOf[Expr[_root_.cats.Traverse[AnyF]]]
              buildTraverse[From, To, A, B](traverse)
            }
          case _ => None
        }
    })

    // --- Handler #2: `NonEmptyMap[A, B] -> NonEmptyMap[C, D]` (needs `cats.kernel.Order[C]`) ---
    registerSpecialCase(new SpecialCaseHandler {
      @scala.annotation.nowarn("msg=is never used")
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): Option[SpecialCasedTransformation[From, To]] =
        (NonEmptyMapCtor.unapply(From), NonEmptyMapCtor.unapply(To)) match {
          case (Some((aArg, bArg)), Some((cArg, dArg))) =>
            import aArg.Underlying as A, bArg.Underlying as B, cArg.Underlying as C, dArg.Underlying as D
            implicit val OrderCType: Type[_root_.cats.kernel.Order[C]] = OrderCtor.apply[C]
            Expr.summonImplicit[_root_.cats.kernel.Order[C]].toOption.map { orderC =>
              buildNonEmptyMap[From, To, A, B, C, D](orderC)
            }
          case _ => None
        }
    })

    // --- Handler #3: `NonEmptySet[A] -> NonEmptySet[B]` (needs `cats.kernel.Order[B]`) ---
    registerSpecialCase(new SpecialCaseHandler {
      @scala.annotation.nowarn("msg=is never used")
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): Option[SpecialCasedTransformation[From, To]] =
        (NonEmptySetCtor.unapply(From), NonEmptySetCtor.unapply(To)) match {
          case (Some(aArg), Some(bArg)) =>
            import aArg.Underlying as A, bArg.Underlying as B
            implicit val OrderBType: Type[_root_.cats.kernel.Order[B]] = OrderCtor.apply[B]
            Expr.summonImplicit[_root_.cats.kernel.Order[B]].toOption.map { orderB =>
              buildNonEmptySet[From, To, A, B](orderB)
            }
          case _ => None
        }
    })

    // --- Handler #4: `F[A] -> G[B]` via `cats.Traverse[F]` + `cats.arrow.FunctionK[F, G]` (`F ~> G`) ---
    registerSpecialCase(new SpecialCaseHandler {
      @scala.annotation.nowarn("msg=is never used")
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): Option[SpecialCasedTransformation[From, To]] =
        (Type.decompose1[From], Type.decompose1[To]) match {
          case (Some((fCtor, fArg)), Some((gCtor, gArg))) =>
            import fArg.Underlying as A, gArg.Underlying as B
            implicit val TraverseFType: Type[_root_.cats.Traverse[AnyK1]] = TraverseCtor.apply(using fCtor)
            // Build `Type[FunctionK[F, G]]` by untyped application (there is no `CtorK2` for kind `(*->*, *->*) -> *`).
            val functionKFG = UntypedType.applyTypeArgs(
              UntypedType.typeConstructor(FunctionKSample.asUntyped),
              List(fCtor.asUntyped, gCtor.asUntyped)
            )
            implicit val FunctionKType: Type[_root_.cats.arrow.FunctionK[AnyK1, AnyK1]] =
              UntypedType.toTyped[_root_.cats.arrow.FunctionK[AnyK1, AnyK1]](functionKFG)
            (
              Expr.summonImplicit[_root_.cats.Traverse[AnyK1]].toOption,
              Expr.summonImplicit[_root_.cats.arrow.FunctionK[AnyK1, AnyK1]].toOption
            ) match {
              case (Some(traverseExpr), Some(functionKExpr)) =>
                // Retype the summoned instances onto erased, quote-safe shapes (meta-level `Expr` casts).
                val traverse = traverseExpr.asInstanceOf[Expr[_root_.cats.Traverse[AnyF]]]
                val functionK =
                  functionKExpr.asInstanceOf[Expr[_root_.cats.arrow.FunctionK[_root_.cats.Id, _root_.cats.Id]]]
                Some(buildFunctionK[From, To, A, B](traverse, functionK))
              case _ => None
            }
          case _ => None
        }
    })
  }
}
