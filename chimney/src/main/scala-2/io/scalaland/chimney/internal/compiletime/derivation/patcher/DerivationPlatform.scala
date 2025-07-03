package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait DerivationPlatform
    extends Derivation
    with transformer.DerivationPlatform
    with rules.PatchImplicitRuleModule
    with rules.PatchSubtypeRuleModule
    with rules.PatchOptionWithOptionOptionRuleModule
    with rules.PatchEitherWithOptionEitherRuleModule
    with rules.PatchCollectionWithOptionCollectionRuleModule
    with rules.PatchOptionWithNonOptionRuleModule
    with rules.PatchProductWithProductRuleModule
    with rules.PatchNotMatchedRuleModule {

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

  private val ignoredPatcherImplicits = makeIgnored[io.scalaland.chimney.Patcher.type](
    "derive" // handled by recursion in macro
  )

  override protected def summonPatcherUnchecked[A: Type, Patch: Type]
      : Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    summonIgnoring[io.scalaland.chimney.Patcher[A, Patch]](ignoredPatcherImplicits*)

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
