package io.scalaland.chimney.internal.compiletime.derivation.iso

import io.scalaland.chimney.dsl
import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{Derivation, Gateway}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class IsoMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveIsoWithDefaults[
      First: WeakTypeTag,
      Second: WeakTypeTag
  ]: c.universe.Expr[Iso[First, Second]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying
      c.Expr(
        q"""
        io.scalaland.chimney.Iso[${Type[First]}, ${Type[Second]}](
          first = ${deriveTotalTransformer[
            First,
            Second,
            runtime.TransformerOverrides.Empty,
            runtime.TransformerFlags.Default,
            implicitScopeFlagsType.Underlying
          ](ChimneyExpr.RuntimeDataStore.empty)},
          second = ${deriveTotalTransformer[
            Second,
            First,
            runtime.TransformerOverrides.Empty,
            runtime.TransformerFlags.Default,
            implicitScopeFlagsType.Underlying
          ](ChimneyExpr.RuntimeDataStore.empty)}
        )
        """
      )
    }
  )

  def deriveIsoWithConfig[
      First: WeakTypeTag,
      Second: WeakTypeTag,
      FirstOverrides <: runtime.TransformerOverrides: WeakTypeTag,
      SecondOverrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[Iso[First, Second]] = retypecheck {
    val iso = c.Expr[Iso[First, Second]](
      q"""
        io.scalaland.chimney.Iso[${Type[First]}, ${Type[Second]}](
          first = ${deriveTotalTransformer[
          First,
          Second,
          FirstOverrides,
          InstanceFlags,
          ImplicitScopeFlags
        ](
          // Called by IsoDefinition => prefix is IsoDefinition
          c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.first.runtimeData")
        )},
          second = ${deriveTotalTransformer[
          Second,
          First,
          SecondOverrides,
          InstanceFlags,
          ImplicitScopeFlags
        ](
          // Called by IsoDefinition => prefix is IsoDefinition
          c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.second.runtimeData")
        )}
          )
        """
    )
    c.Expr[Iso[First, Second]](q"{ ${Expr.suppressUnused(tc)}; $iso }")
  }

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

    val body = useImplicitScopeFlags(implicitScopeFlagsType)
    c.Expr[A](q"{ ${Expr.suppressUnused(implicitScopeConfig)}; $body }")
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
