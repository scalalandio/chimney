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

  private val Transformer_derive =
    Symbol.classSymbol("io.scalaland.chimney.Transformer").companionModule.methodMember("derive")
  private val Transformer_transformerFromIsoFirst =
    Symbol.classSymbol("io.scalaland.chimney.Transformer").companionModule.methodMember("transformerFromIsoFirst")
  private val Transformer_transformerFromIsoSecond =
    Symbol.classSymbol("io.scalaland.chimney.Transformer").companionModule.methodMember("transformerFromIsoSecond")
  private val Transformer_transformerFromCodecEncoder =
    Symbol.classSymbol("io.scalaland.chimney.Transformer").companionModule.methodMember("transformerFromCodecEncoder")
  private val ignoredTransformerImplicits =
    Transformer_derive ++ Transformer_transformerFromIsoFirst ++ Transformer_transformerFromIsoSecond ++ Transformer_transformerFromCodecEncoder

  override protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    scala.quoted.Expr.summonIgnoring[io.scalaland.chimney.Transformer[From, To]](ignoredTransformerImplicits*)
  // TODO: manual fallback on Codec/Iso

  private val PartialTransformer_derive =
    Symbol.classSymbol("io.scalaland.chimney.PartialTransformer").companionModule.methodMember("derive")
  private val PartialTransformer_partialTransformerFromCodecDecoder =
    Symbol
      .classSymbol("io.scalaland.chimney.PartialTransformer")
      .companionModule
      .methodMember("partialTransformerFromCodecDecoder")
  private val PartialTransformer_liftTotal =
    Symbol.classSymbol("io.scalaland.chimney.PartialTransformer").companionModule.methodMember("liftTotal")
  private val ignoredPartialTransformerImplicits =
    PartialTransformer_derive ++ PartialTransformer_partialTransformerFromCodecDecoder ++ PartialTransformer_liftTotal

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
