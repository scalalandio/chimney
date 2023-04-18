package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.TransformerDerivationError
import io.scalaland.chimney.internal.compiletime.{
  ConfigurationsPlatform,
  Contexts,
  DefinitionsPlatform,
  DerivationError,
  DerivationErrors,
  DerivationResult
}
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros

private[compiletime] trait LegacyPlatform extends Legacy {
  this: DefinitionsPlatform & ConfigurationsPlatform & Contexts =>

  protected object legacy extends LegacyImpl {

    private val oldMacros = new TransformerBlackboxMacros(c)

    private def convertToLegacyConfig[From, To](implicit
        ctx: TransformerContext[From, To]
    ): oldMacros.TransformerConfig = {
      // TODO: convert our new config into our old config
      ???
    }

    private def convertToLegacyType[T: Type]: oldMacros.c.Type = Type[T].asInstanceOf[oldMacros.c.universe.Type]

    private def convertFromLegacyDerivedTree[From, To](
        derivedTree: Either[Seq[TransformerDerivationError], oldMacros.DerivedTree]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] = derivedTree match {
      case Left(oldErrors) =>
        DerivationResult.fail(
          DerivationErrors(
            DerivationError.TransformerError(oldErrors.head),
            oldErrors.tail.map(DerivationError.TransformerError).toSeq*
          )
        )
      case Right(oldMacros.DerivedTree(tree, _: oldMacros.DerivationTarget.TotalTransformer.type)) =>
        DerivationResult.pure(DerivedExpr.TotalExpr(c.Expr[To](tree.asInstanceOf[c.Tree])(c.WeakTypeTag(Type[To]))))
      case Right(oldMacros.DerivedTree(tree, _: oldMacros.DerivationTarget.PartialTransformer)) =>
        DerivationResult.pure(
          DerivedExpr.TotalExpr(c.Expr[To](tree.asInstanceOf[c.Tree])(c.WeakTypeTag(ChimneyType.PartialResult[To])))
        )
    }

    override def deriveTransformerTargetExprWithOldMacros[From, To](implicit
        ctx: TransformerContext[From, To]
    ): DerivationResult[DerivedExpr[To]] = DerivationResult {
      oldMacros.resolveTransformerBody(convertToLegacyConfig)(convertToLegacyType[From], convertToLegacyType[To])
    }.flatMap(convertFromLegacyDerivedTree[From, To](_))
  }
}
