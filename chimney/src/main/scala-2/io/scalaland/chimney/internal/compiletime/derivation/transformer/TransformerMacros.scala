package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.TransformerCfg.Empty
import io.scalaland.chimney.internal.TransformerFlags.Default
import io.scalaland.chimney.{internal, PartialTransformer, Transformer}

import scala.reflect.macros.blackbox

final class TransformerMacros(val c: blackbox.Context) extends DerivationPlatform with GatewayPlatform {

  type ImplicitScopeFlagsType <: internal.TransformerFlags

  final def deriveTotalTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[Transformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType =>
      import typeUtils.fromWeakConversion.*
      deriveTotalTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](
        runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty
      )
    }

  final def derivePartialTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType =>
      import typeUtils.fromWeakConversion.*
      derivePartialTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](runtimeDataStore =
        ChimneyExpr.RuntimeDataStore.empty
      )
    }

  private def findImplicitScopeTransformerConfiguration: c.universe.Tree = {
    import c.universe.*

    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree).getOrElse {
      // $COVERAGE-OFF$
      reportError("Can't locate implicit TransformerConfiguration!")
      // $COVERAGE-ON$
    }
  }

  private def inferImplicitTpe(tpeTree: c.universe.Tree): Option[c.universe.Tree] = {
    val typedTpeTree = c.typecheck(
      tree = tpeTree,
      silent = true,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = false
    )

    scala.util
      .Try(c.inferImplicitValue(typedTpeTree.tpe, silent = true, withMacrosDisabled = false))
      .toOption
      .filterNot(_ == c.universe.EmptyTree)
  }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A](
      useImplicitScopeFlags: Type[ImplicitScopeFlagsType] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = findImplicitScopeTransformerConfiguration
    val implicitScopeConfigType =
      typeUtils.fromUntyped(implicitScopeConfig.tpe.typeArgs.head).asInstanceOf[Type[ImplicitScopeFlagsType]]

    import c.universe.*
    c.Expr[A](
      q"""
          val _ = $implicitScopeConfig
          ${useImplicitScopeFlags(implicitScopeConfigType)}
       """
    )
  }
}
