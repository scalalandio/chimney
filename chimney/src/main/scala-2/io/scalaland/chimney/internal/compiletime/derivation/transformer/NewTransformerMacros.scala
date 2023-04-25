package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.reflect.macros.blackbox

final class NewTransformerMacros(val c: blackbox.Context)
    extends DefinitionsPlatform
    with DerivationPlatform
    with GatewayPlatform {

  def deriveTotalTransformerImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[Transformer[From, To]] = {
    import typeUtils.fromWeakConversion.*

    val scopedFlagsTypeTree = findLocalTransformerConfigurationFlags
    val scopedFlagsTpe = scopedFlagsTypeTree.tpe.typeArgs.head

    val expr = deriveTotalTransformer(
      Type[From],
      Type[To],
      ChimneyType.TransformerCfg.Empty,
      ChimneyType.TransformerFlags.Default,
      typeUtils.fromUntyped(scopedFlagsTpe)
    )

//    println(Expr.prettyPrint(expr))

    expr
  }

  def findLocalTransformerConfigurationFlags: c.universe.Tree = {
    import c.universe.*

    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree)
      .getOrElse {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Can't locate implicit TransformerConfiguration!")
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
}
