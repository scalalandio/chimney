package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.internal.NotSupportedTransformerDerivation
import io.scalaland.chimney.partial

trait ResultOps { this: Definitions & Derivation =>

  implicit class DerivationResultModule(derivationResult: DerivationResult.type) {

    def totalExpr[To](expr: Expr[To]): DerivationResult[Rule.ExpansionResult.Expanded[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(DerivedExpr.TotalExpr[To](expr)))

    def partialExpr[To](expr: Expr[partial.Result[To]]): DerivationResult[Rule.ExpansionResult.Expanded[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(DerivedExpr.PartialExpr[To](expr)))

    def continue[A]: DerivationResult[Rule.ExpansionResult[A]] =
      DerivationResult.pure(Rule.ExpansionResult.Continue)

    def notSupportedTransformerDerivation[From, To, T](implicit
        ctx: TransformerContext[From, To]
    ): DerivationResult[T] =
      DerivationResult.transformerError(
        NotSupportedTransformerDerivation(
          fieldName = Expr.prettyPrint(ctx.src),
          sourceTypeName = Type.prettyPrint[From],
          targetTypeName = Type.prettyPrint[To]
        )
      )
  }

}
