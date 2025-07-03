package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitionsPlatform}

trait DerivationPlatform
    extends Derivation
    with ChimneyDefinitionsPlatform
    with datatypes.IterableOrArraysPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformImplicitPartialFallbackToTotalRuleModule
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

  import c.universe.*

  private def summonIgnoring[A: c.WeakTypeTag](ignored: Symbol*): Option[Expr[A]] = scala.util
    .Try {
      c.inferImplicitValueIgnoring(weakTypeOf[A], silent = true, withMacrosDisabled = false)(ignored*)
    }
    .toOption
    .filterNot(_ == EmptyTree)
    .map(tree => c.Expr[A](tree))

  private def makeIgnored[A: c.WeakTypeTag](methods: String*): Seq[Symbol] = {
    val module = weakTypeOf[A]
    methods.map { name =>
      val method = module.member(TermName(name))
      assert(method != NoSymbol && method.isMethod && method.isImplicit)
      method
    }
  }

  private val ignoredTransformerImplicits = makeIgnored[io.scalaland.chimney.Transformer.type](
    "derive", // handled by recursion in macro
    "transformerFromIsoFirst", // handled below
    "transformerFromIsoSecond", // handled below
    "transformerFromCodecEncoder" // handled below
  )

  override protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    summonIgnoring[io.scalaland.chimney.Transformer[From, To]](ignoredTransformerImplicits*)
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Iso[From, To]].map(iso => c.Expr(q"$iso.first"))
      }
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Iso[To, From]].map(iso => c.Expr(q"$iso.second"))
      }
      .orElse {
        Expr.summonImplicit[io.scalaland.chimney.Codec[From, To]].map(codec => c.Expr(q"$codec.encode"))
      }

  private val ignoredPartialTransformerImplicits = makeIgnored[io.scalaland.chimney.PartialTransformer.type](
    "derive", // handled by recursion in macro
    "partialTransformerFromCodecDecoder" // handled below
  )

  override protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    summonIgnoring[io.scalaland.chimney.PartialTransformer[From, To]](ignoredPartialTransformerImplicits*).orElse {
      Expr.summonImplicit[io.scalaland.chimney.Codec[To, From]].map(codec => c.Expr(q"$codec.decode"))
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
    TransformToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )
}
