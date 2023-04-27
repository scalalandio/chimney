package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.{internal, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.reflect.macros.blackbox

final class TransformerMacros(val c: blackbox.Context)
    extends DefinitionsPlatform
    with DerivationPlatform
    with GatewayPlatform {

  final def deriveTotalTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[Transformer[From, To]] = {
    import typeUtils.fromWeakConversion.*

    resolveLocalTransformerConfigAndMuteUnusedConfigWarnings(LocalConfugType =>
      deriveTotalTransformer(
        Type[From],
        Type[To],
        ChimneyType.TransformerCfg.Empty,
        ChimneyType.TransformerFlags.Default,
        LocalConfugType
      )
    )
  }

  final def derivePartialTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] = {
    import typeUtils.fromWeakConversion.*

    resolveLocalTransformerConfigAndMuteUnusedConfigWarnings(LocalConfugType =>
      derivePartialTransformer(
        Type[From],
        Type[To],
        ChimneyType.TransformerCfg.Empty,
        ChimneyType.TransformerFlags.Default,
        LocalConfugType
      )
    )
  }

  private def findLocalTransformerConfigurationFlags: c.universe.Tree = {
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

  private def resolveLocalTransformerConfigAndMuteUnusedConfigWarnings[A](
      useLocalConfig: Type[internal.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val localConfig = findLocalTransformerConfigurationFlags
    val localConfigType =
      typeUtils.fromUntyped(localConfig.tpe.typeArgs.head).asInstanceOf[Type[internal.TransformerFlags]]

    import c.universe.*
    c.Expr[A](
      q"""
          val _ = $localConfig
          ${useLocalConfig(localConfigType)}
       """
    )
  }
}
