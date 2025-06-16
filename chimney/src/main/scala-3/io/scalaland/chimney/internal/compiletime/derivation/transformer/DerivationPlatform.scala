package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitionsPlatform
import io.scalaland.chimney.internal.compiletime.datatypes
import io.scalaland.chimney.internal.compiletime.DerivationResult

abstract private[compiletime] class DerivationPlatform(q: scala.quoted.Quotes)
    extends ChimneyDefinitionsPlatform(q)
    with Derivation
    with datatypes.IterableOrArraysPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformImplicitOuterTransformerRuleModule
    with rules.TransformImplicitConversionRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformTypeConstraintRuleModule
    with rules.TransformToSingletonRuleModule
    with rules.TransformValueClassToValueClassRuleModule
    with rules.TransformValueClassToTypeRuleModule
    with rules.TransformTypeToValueClassRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.TransformEitherToEitherRuleModule
    with rules.TransformMapToMapRuleModule
    with rules.TransformIterableToIterableRuleModule
    with rules.TransformProductToProductRuleModule
    with rules.TransformSealedHierarchyToSealedHierarchyRuleModule {

  import quotes.reflect.*
  import scala.quoted.Expr.summonIgnoring

  // With summonIgnoring we should ignore all implicits from the companion and implement implicit priorities ourselves.
  private def makeIgnored(className: String)(methods: String*): Seq[Symbol] = {
    val module = Symbol.classSymbol(className).companionModule
    // assert(!module.isNoSymbol)
    methods.map { name =>
      val method = module.methodMember(name).head
      // assert(!method.isNoSymbol && (method.flags.is(Flags.Implicit) || method.flags.is(Flags.Given)))
      method
    }
  }

  private val ignoredTransformerImplicits = makeIgnored("io.scalaland.chimney.Transformer")(
    "derive", // handled by recursion in macro
    "transformerFromIsoFirst", // handled below
    "transformerFromIsoSecond", // handled below
    "transformerFromCodecEncoder" // handled below
  )

  override protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    summonIgnoring[io.scalaland.chimney.Transformer[From, To]](ignoredTransformerImplicits*)
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Iso[From, To]].map(iso => '{ ${ iso }.first })
      }
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Iso[To, From]].map(iso => '{ ${ iso }.second })
      }
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Codec[From, To]].map(codec => '{ ${ codec }.encode })
      }

  private val ignoredPartialTransformerImplicits = makeIgnored("io.scalaland.chimney.PartialTransformer")(
    "derive", // handled by recursion in macro
    "partialTransformerFromCodecDecoder" // handled below
  )

  override protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    summonIgnoring[io.scalaland.chimney.PartialTransformer[From, To]](ignoredPartialTransformerImplicits*).orElse {
      Expr.summonImplicit[io.scalaland.chimney.Codec[To, From]].map(codec => '{ ${ codec }.decode })
    }

  // TODO: Move to dedicated file
  protected object TransformImplicitPartialFallbackToTotalRule extends Rule("PartialFallbackToTotal") {
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if ctx.config.areLocalFlagsAndOverridesEmpty then transformWithImplicitIfAvailable[From, To]
      else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithImplicitIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(_)        => DerivationResult.attemptNextRule
      case TransformationContext.ForPartial(src, _) =>
        summonTransformerUnchecked[From, To].fold(DerivationResult.attemptNextRule[To]) { totalTransformer =>
          // We're constructing:
          // '{ ${ totalTransformer }.transform(${ src }) } }
          DerivationResult.expandedTotal(totalTransformer.transform(src))
        }
    }
  }

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformImplicitPartialFallbackToTotalRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    TransformSubtypesRule,
    TransformTypeConstraintRule,
    TransformToSingletonRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )
}
