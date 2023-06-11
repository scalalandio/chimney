package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.internal.NotSupportedTransformerDerivation
import io.scalaland.chimney.partial

private[compiletime] trait ResultOps { this: Definitions & Derivation =>

  implicit protected class DerivationResultModule(derivationResult: DerivationResult.type) {

    def expanded[To](expr: TransformationExpr[To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(expr))

    def expandedTotal[To](expr: Expr[To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(TransformationExpr.TotalExpr[To](expr)))

    def expandedPartial[To](expr: Expr[partial.Result[To]]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(TransformationExpr.PartialExpr[To](expr)))

    def attemptNextRule[A]: DerivationResult[Rule.ExpansionResult[A]] =
      DerivationResult.pure(Rule.ExpansionResult.AttemptNextRule)

    def notSupportedTransformerDerivation[From, To, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedTransformerDerivation(
        fieldName = ctx.src.prettyPrint,
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def summonImplicit[A: Type]: DerivationResult[Expr[A]] = Expr
      .summonImplicit[A]
      .fold(
        // TODO: create separate type for missing implicit
        DerivationResult.assertionError[Expr[A]](s"Implicit not found: ${Type.prettyPrint[A]}")
      )(DerivationResult.pure[Expr[A]](_))
  }
}
