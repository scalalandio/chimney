package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.TransformerDerivationError
import io.scalaland.chimney.internal.compiletime.*
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait LegacyPlatform extends Legacy {
  this: DefinitionsPlatform & ConfigurationsPlatform & Contexts =>

  protected object legacy extends LegacyImpl {

    private val oldMacros = new TransformerBlackboxMacros(c)

    private def convertToLegacyConfig[From, To](implicit
        ctx: TransformerContext[From, To]
    ): oldMacros.TransformerConfig = {
      oldMacros.TransformerConfig(
        srcPrefixTree = ctx.src.tree.asInstanceOf[oldMacros.c.Tree],
        derivationTarget = ctx match {
          case _: TransformerContext.ForTotal[?, ?] => oldMacros.DerivationTarget.TotalTransformer
          case a: TransformerContext.ForPartial[?, ?] =>
            oldMacros.DerivationTarget.PartialTransformer(a.failFast.tree.asInstanceOf[oldMacros.c.TermName])
        },
        flags = {

          oldMacros.TransformerFlags(
            methodAccessors = ctx.config.flags.methodAccessors,
            processDefaultValues = ctx.config.flags.processDefaultValues,
            beanSetters = ctx.config.flags.beanSetters,
            beanGetters = ctx.config.flags.beanGetters,
            optionDefaultsToNone = ctx.config.flags.optionDefaultsToNone,
            implicitConflictResolution = ctx.config.flags.implicitConflictResolution
          )
        },
        fieldOverrides = ctx.config.legacy.fieldOverrideLegacy.map {
          case (key, FieldOverride.RuntimeConfiguration.Const(valueOf)) =>
            key -> oldMacros.FieldOverride.Const(valueOf)
          case (key, FieldOverride.RuntimeConfiguration.Computed(valueOf)) =>
            key -> oldMacros.FieldOverride.Computed(valueOf)
          case (key, FieldOverride.RuntimeConfiguration.ConstPartial(valueOf)) =>
            key -> oldMacros.FieldOverride.ConstPartial(valueOf)
          case (key, FieldOverride.RuntimeConfiguration.ComputedPartial(valueOf)) =>
            key -> oldMacros.FieldOverride.ComputedPartial(valueOf)
          case (key, FieldOverride.RuntimeConfiguration.RenamedFrom(valueOf)) =>
            key -> oldMacros.FieldOverride.RenamedFrom(valueOf)
        },
        coproductInstanceOverrides =
          ctx.config.legacy.coproductInstanceOverridesLegacy.map { case ((type1, type2), int) =>
            ((type1.Type.typeSymbol.asInstanceOf[oldMacros.c.Symbol], type2.Type.asInstanceOf[oldMacros.c.Type]), int)
          },
        coproductInstancesPartialOverrides =
          ctx.config.legacy.coproductInstancesPartialOverridesLegacy.map { case ((type1, type2), int) =>
            ((type1.Type.typeSymbol.asInstanceOf[oldMacros.c.Symbol], type2.Type.asInstanceOf[oldMacros.c.Type]), int)
          },
        transformerDefinitionPrefix = ctx.config.legacy.transformerDefinitionPrefix.tree.asInstanceOf[oldMacros.c.Tree],
        definitionScope = ctx.config.legacy.definitionScope.asInstanceOf[Option[(oldMacros.c.Type, oldMacros.c.Type)]]
      )
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
