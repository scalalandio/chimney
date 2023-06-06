package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.TransformerDerivationError
import io.scalaland.chimney.internal.compiletime.{DerivationError, DerivationErrors, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.DerivationPlatform
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait LegacyMacrosFallbackRuleModule { this: DerivationPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import ChimneyTypeImplicits.*

  // TODO: remove this rule once all rules are migrated; it's here only to make the Scala 2 tests pass during migration
  protected object LegacyMacrosFallbackRule extends Rule("LegacyMacrosFallback") {

    override def expand[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      for {
        cfg <- DerivationResult(convertToLegacyConfig)
        legacyBody = oldMacros.resolveTransformerBody(cfg)(convertToLegacyType[From], convertToLegacyType[To])
        body <- convertFromLegacyDerivedTree[From, To](legacyBody)
      } yield initializeFailFastIfNeeded(body, cfg.derivationTarget, ctx)
    }

    private val oldMacros = new TransformerBlackboxMacros(c)

    private def convertToLegacyConfig[From, To](implicit
        ctx: TransformationContext[From, To]
    ): oldMacros.TransformerConfig = {
      oldMacros.TransformerConfig(
        srcPrefixTree = ctx.src.tree.asInstanceOf[oldMacros.c.Tree],
        derivationTarget = ctx.fold[oldMacros.DerivationTarget] { _ =>
          oldMacros.DerivationTarget.TotalTransformer
        } {
          // THIS ONE CREATE FRESH TERM THAT WE HAVE TO INITIALIZE!
          _ => oldMacros.DerivationTarget.PartialTransformer()
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
        transformerDefinitionPrefix = ctx.runtimeDataStore.tree match {
          case q"$td.runtimeData" => td.asInstanceOf[oldMacros.c.Tree]
          case _                  => q"null".asInstanceOf[oldMacros.c.Tree]
        },
        definitionScope =
          ctx.config.preventResolutionForTypes.asInstanceOf[Option[(oldMacros.c.Type, oldMacros.c.Type)]]
      )
    }

    private def convertToLegacyType[T: Type]: oldMacros.c.Type = Type[T].asInstanceOf[oldMacros.c.universe.Type]

    private def convertFromLegacyDerivedTree[From, To](
        derivedTree: Either[Seq[TransformerDerivationError], oldMacros.DerivedTree]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = derivedTree match {
      case Left(oldErrors) =>
        DerivationResult.fail(
          DerivationErrors(
            DerivationError.TransformerError(oldErrors.head),
            oldErrors.tail.map(DerivationError.TransformerError)*
          )
        )
      case Right(oldMacros.DerivedTree(tree, _: oldMacros.DerivationTarget.TotalTransformer.type)) =>
        DerivationResult.totalExpr(Expr.platformSpecific.asExpr[To](tree.asInstanceOf[c.Tree]))
      case Right(oldMacros.DerivedTree(tree, _: oldMacros.DerivationTarget.PartialTransformer)) =>
        DerivationResult.partialExpr(Expr.platformSpecific.asExpr[partial.Result[To]](tree.asInstanceOf[c.Tree]))
    }

    private def initializeFailFastIfNeeded[To: Type](
        result: Rule.ExpansionResult[To],
        dt: oldMacros.DerivationTarget,
        ctx: TransformationContext[?, ?]
    ): Rule.ExpansionResult[To] = result match {
      case Rule.ExpansionResult.Expanded(TransformationExpr.PartialExpr(expr)) =>
        val termName =
          dt.asInstanceOf[oldMacros.DerivationTarget.PartialTransformer].failFastTermName.asInstanceOf[c.TermName]
        val failFastValue = ctx.asInstanceOf[TransformationContext.ForPartial[?, ?]].failFast
        import c.universe.{internal as _, Transformer as _, Expr as _, *}
        Rule.ExpansionResult.Expanded(
          TransformationExpr.PartialExpr(
            Expr.platformSpecific.asExpr[partial.Result[To]](
              q"""
                val $termName: scala.Boolean = $failFastValue
                val _ = $termName
                $expr
              """
            )
          )
        )
      case els => els
    }
  }
}
