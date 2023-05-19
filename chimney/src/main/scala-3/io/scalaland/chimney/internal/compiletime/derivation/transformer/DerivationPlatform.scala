package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

abstract private[derivation] class DerivationPlatform(q: scala.quoted.Quotes)
    extends DefinitionsPlatform(using q)
    with Derivation
    with ImplicitSummoningPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.NotImplementedFallbackRuleModule {

  final override protected def deriveFold[NewFrom: Type, NewTo: Type, To: Type](
      deriveFromSrc: Expr[NewFrom] => DerivationResult[Rule.ExpansionResult[NewTo]]
  )(
      provideSrcForTotal: (Expr[NewFrom] => Expr[NewTo]) => DerivedExpr[To]
  )(
      provideSrcForPartial: (Expr[NewFrom] => Expr[partial.Result[NewTo]]) => DerivedExpr[To]
  ): DerivationResult[Rule.ExpansionResult[To]] =
    DerivationResult.notYetImplemented("deriveFrom")

  final override protected val rulesAvailableForPlatform: List[Rule] =
    List(TransformImplicitRule, TransformSubtypesRule, TransformOptionToOptionRule, NotImplementedFallbackRule)
}
