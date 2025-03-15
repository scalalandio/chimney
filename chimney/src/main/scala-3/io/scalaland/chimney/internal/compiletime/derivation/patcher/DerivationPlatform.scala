package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

abstract private[compiletime] class DerivationPlatform(q: scala.quoted.Quotes)
    extends transformer.DerivationPlatform(q)
    with Derivation
    with rules.PatchImplicitRuleModule
    with rules.PatchSubtypeRuleModule
    with rules.PatchOptionWithOptionOptionRuleModule
    with rules.PatchEitherWithOptionEitherRuleModule
    with rules.PatchCollectionWithOptionCollectionRuleModule
    with rules.PatchOptionWithNonOptionRuleModule
    with rules.PatchProductWithProductRuleModule
    with rules.PatchNotMatchedRuleModule {

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

  private val ignoredPatcherImplicits = makeIgnored("io.scalaland.chimney.Patcher")(
    "derive"
  )

  override protected def summonPatcherUnchecked[A: Type, Patch: Type]
      : Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    scala.quoted.Expr.summonIgnoring[io.scalaland.chimney.Patcher[A, Patch]](ignoredPatcherImplicits*)

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    PatchImplicitRule,
    TransformImplicitRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    PatchSubtypeRuleModule,
    TransformTypeConstraintRule,
    PatchOptionWithNonOptionRule,
    PatchOptionWithOptionOptionRule,
    PatchEitherWithOptionEitherRule,
    PatchCollectionWithOptionCollectionRule,
    TransformOptionToOptionRule,
    TransformToOptionRule,
    TransformToSingletonRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformSubtypesRule,
    PatchProductWithProductRule,
    TransformSealedHierarchyToSealedHierarchyRule,
    PatchNotMatchedRule
  )
}
