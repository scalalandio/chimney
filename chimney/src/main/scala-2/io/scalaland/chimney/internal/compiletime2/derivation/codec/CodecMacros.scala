package io.scalaland.chimney.internal.compiletime2.derivation.codec

import io.scalaland.chimney.dsl
import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.compiletime2.PlatformBridge
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.{Derivation, Gateway}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

/** Hearth-based port of `...compiletime.derivation.codec.CodecMacros` (Scala 2).
  *
  * Public methods (names, signatures, type params) mirror the old macro bundle 1:1 so that the binding sites in
  * `io.scalaland.chimney.dsl.*` can flip packages mechanically in the next phase.
  *
  * Differences vs the old version: same as
  * [[io.scalaland.chimney.internal.compiletime2.derivation.transformer.TransformerMacros]] - extends
  * [[PlatformBridge]] + the now-shared transformer `Derivation`/`Gateway` instead of the old per-platform
  * `DerivationPlatform`, `Expr.block` -> `blockExpr`, `Type.platformSpecific.fromUntyped[A](tpe)` ->
  * `c.WeakTypeTag[A](tpe)`, `?<`/`.as_?<` -> `??<:`/`.as_??<:`, `Expr.summonImplicit(...)` -> `.toOption` added.
  */
final class CodecMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveCodecWithDefaults[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag
  ]: c.universe.Expr[Codec[Domain, Dto]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying
      c.Expr(
        q"""
        io.scalaland.chimney.Codec[${Type[Domain]}, ${Type[Dto]}](
          encode = ${deriveTotalTransformer[
            Domain,
            Dto,
            runtime.TransformerOverrides.Empty,
            runtime.TransformerFlags.Default,
            implicitScopeFlagsType.Underlying
          ](ChimneyExpr.RuntimeDataStore.empty)},
          decode = ${derivePartialTransformer[
            Dto,
            Domain,
            runtime.TransformerOverrides.Empty,
            runtime.TransformerFlags.Default,
            implicitScopeFlagsType.Underlying
          ](ChimneyExpr.RuntimeDataStore.empty)}
        )
        """
      )
    }
  )

  def deriveCodecWithConfig[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag,
      EncodeOverrides <: runtime.TransformerOverrides: WeakTypeTag,
      DecodeOverrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[Codec[Domain, Dto]] = retypecheck(
    blockExpr(
      List(Expr.suppressUnused(tc)),
      c.Expr(
        q"""
        io.scalaland.chimney.Codec[${Type[Domain]}, ${Type[Dto]}](
          encode = ${deriveTotalTransformer[
            Domain,
            Dto,
            EncodeOverrides,
            InstanceFlags,
            ImplicitScopeFlags
          ](
            // Called by CodecDefinition => prefix is CodecDefinition
            c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.encode.runtimeData")
          )},
          decode = ${derivePartialTransformer[
            Dto,
            Domain,
            DecodeOverrides,
            InstanceFlags,
            ImplicitScopeFlags
          ](
            // Called by CodecDefinition => prefix is CodecDefinition
            c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.decode.runtimeData")
          )}
        )
        """
      )
    )
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val transformerConfigurationType =
        c.WeakTypeTag[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(transformerConfigurationType).toOption.getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = c
      .WeakTypeTag[runtime.TransformerFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_??<:[runtime.TransformerFlags]

    blockExpr(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
