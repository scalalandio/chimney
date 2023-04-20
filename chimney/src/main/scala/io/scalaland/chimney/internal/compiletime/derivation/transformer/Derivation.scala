package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait Derivation { this: Definitions =>

  /** Intended use case: recursive derivation */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] = Rule[From, To]

  abstract protected class Rule {

    def isApplicableTo[From, To](ctx: TransformerContext[From, To]): Boolean

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]]
  }
  protected object Rule {

    def apply[From, To](
        rules: Seq[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      rules.find(_.isApplicableTo(ctx)) match {
        case Some(rule) => rule[From, To]
        case None       => DerivationResult.notSupportedTransformerDerivation
      }

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      apply(rulesAvailableForPlatform)
  }

  protected def instantiateTotalTransformer[From: Type, To: Type](
      f: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[Transformer[From, To]]]

  protected def instantiatePartialTransformer[From: Type, To: Type](
      f: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[partial.Result[To]]]
  ): DerivationResult[Expr[PartialTransformer[From, To]]]

  protected val rulesAvailableForPlatform: Seq[Rule]
}
