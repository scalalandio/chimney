package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.TransformerDerivationError
import io.scalaland.chimney.internal.compiletime.{
  DefinitionsPlatform,
  DerivationError,
  DerivationErrors,
  DerivationResult
}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.DerivationPlatform
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait LegacyMacrosFallbackRuleModule {
  this: DefinitionsPlatform & DerivationPlatform =>

  protected object LegacyMacrosFallbackRule extends Rule {

    // we want this fallback to ALWAYS work until we no longer need it
    override def isApplicableTo[From, To](implicit ctx: TransformerContext[From, To]): Boolean = true

    override def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      DerivationResult.log(s"Matched fallback to legacy macros derivation rule") >>
        DerivationResult {
          oldMacros.resolveTransformerBody(convertToLegacyConfig)(convertToLegacyType[From], convertToLegacyType[To])
        }.flatMap(convertFromLegacyDerivedTree[From, To](_))

    private val oldMacros = new TransformerBlackboxMacros(c)

    private def convertToLegacyConfig[From, To](implicit
        ctx: TransformerContext[From, To]
    ): oldMacros.TransformerConfig = {
      oldMacros.TransformerConfig(
        srcPrefixTree = ctx.src.tree.asInstanceOf[oldMacros.c.Tree],
        derivationTarget = ctx match {
          case _: TransformerContext.ForTotal[?, ?] => oldMacros.DerivationTarget.TotalTransformer
          case a: TransformerContext.ForPartial[?, ?] =>
            oldMacros.DerivationTarget.PartialTransformer(
              a.failFast.tree.asInstanceOf[oldMacros.c.universe.Ident].name.toTermName
            )
        },
        flags = oldMacros.TransformerFlags(
          methodAccessors = ctx.config.flags.methodAccessors,
          processDefaultValues = ctx.config.flags.processDefaultValues,
          beanSetters = ctx.config.flags.beanSetters,
          beanGetters = ctx.config.flags.beanGetters,
          optionDefaultsToNone = ctx.config.flags.optionDefaultsToNone,
          implicitConflictResolution = ctx.config.flags.implicitConflictResolution
        ),
        fieldOverrides = ctx.config.fieldOverrides.map {
          case (key, RuntimeFieldOverride.Const(idx)) =>
            key -> oldMacros.FieldOverride.Const(idx)
          case (key, RuntimeFieldOverride.Computed(idx)) =>
            key -> oldMacros.FieldOverride.Computed(idx)
          case (key, RuntimeFieldOverride.ConstPartial(idx)) =>
            key -> oldMacros.FieldOverride.ConstPartial(idx)
          case (key, RuntimeFieldOverride.ComputedPartial(idx)) =>
            key -> oldMacros.FieldOverride.ComputedPartial(idx)
          case (key, RuntimeFieldOverride.RenamedFrom(name)) =>
            key -> oldMacros.FieldOverride.RenamedFrom(name)
        },
        coproductInstanceOverrides =
          ctx.config.coproductOverrides.collect { case ((ct1, ct2), RuntimeCoproductOverride.CoproductInstance(idx)) =>
            (ct1.Type.typeSymbol.asInstanceOf[oldMacros.c.Symbol], ct2.Type.asInstanceOf[oldMacros.c.Type]) -> idx
          },
        coproductInstancesPartialOverrides = ctx.config.coproductOverrides.collect {
          case ((ct1, ct2), RuntimeCoproductOverride.CoproductInstancePartial(idx)) =>
            (ct1.Type.typeSymbol.asInstanceOf[oldMacros.c.Symbol], ct2.Type.asInstanceOf[oldMacros.c.Type]) -> idx
        },
        transformerDefinitionPrefix = Option(ctx.config.legacy.transformerDefinitionPrefix)
          .map(_.tree)
          .getOrElse(oldMacros.c.universe.EmptyTree)
          .asInstanceOf[oldMacros.c.Tree],
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
          DerivedExpr.PartialExpr(
            c.Expr[partial.Result[To]](tree.asInstanceOf[c.Tree])(c.WeakTypeTag(ChimneyType.PartialResult[To]))
          )
        )
    }
  }
}
