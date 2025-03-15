package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitionsPlatform
import io.scalaland.chimney.internal.compiletime.datatypes

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
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.TransformValueClassToValueClassRuleModule
    with rules.TransformValueClassToTypeRuleModule
    with rules.TransformTypeToValueClassRuleModule
    with rules.TransformEitherToEitherRuleModule
    with rules.TransformMapToMapRuleModule
    with rules.TransformIterableToIterableRuleModule
    with rules.TransformProductToProductRuleModule
    with rules.TransformSealedHierarchyToSealedHierarchyRuleModule {

  import quotes.reflect.*

  private def makeIgnored(className: String)(methods: String*): Seq[Symbol] = {
    val module = Symbol.classSymbol(className).companionModule
    assert(!module.isNoSymbol)
    methods.map { name =>
      val method = module.methodMember(name).head
      assert(!method.isNoSymbol && method.flags.is(Flags.Implicit))
      method
    }
  }

  private val ignoredTransformerImplicits = makeIgnored("io.scalaland.chimney.Transformer")(
    "derive",
    "transformerFromIsoFirst",
    "transformerFromIsoSecond",
    "transformerFromCodecEncoder"
  )

  override protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    scala.quoted.Expr.summonIgnoring[io.scalaland.chimney.Transformer[From, To]](ignoredTransformerImplicits*)
  // TODO: manual fallback on Codec/Iso

  private val ignoredPartialTransformerImplicits = makeIgnored("io.scalaland.chimney.PartialTransformer")(
    "derive",
    "partialTransformerFromCodecDecoder",
    "liftTotal"
  )

  override protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    scala.quoted.Expr
      .summonIgnoring[io.scalaland.chimney.PartialTransformer[From, To]](ignoredPartialTransformerImplicits*)
  // TODO: manual fallback on Codec/Iso

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    TransformSubtypesRule,
    TransformTypeConstraintRule,
    TransformToSingletonRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )
}
