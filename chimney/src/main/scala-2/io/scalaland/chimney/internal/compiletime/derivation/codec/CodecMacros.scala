package io.scalaland.chimney.internal.compiletime.derivation.codec

import io.scalaland.chimney.dsl
import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{DerivationPlatform, Gateway}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class CodecMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveCodecWithDefaults[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag
  ]: c.universe.Expr[Codec[Domain, Dto]] =
    retypecheck(
      suppressWarnings(
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
    suppressWarnings(
      Expr.block(
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
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val transformerConfigurationType = Type.platformSpecific
        .fromUntyped[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(transformerConfigurationType).getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = Type.platformSpecific
      .fromUntyped[runtime.TransformerFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_?<[runtime.TransformerFlags]

    Expr.block(
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
