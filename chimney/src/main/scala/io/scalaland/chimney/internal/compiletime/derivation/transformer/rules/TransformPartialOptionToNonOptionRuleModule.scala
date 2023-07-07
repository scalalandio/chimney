package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From]) match {
        case (TransformationContext.ForPartial(src, _), Type.Option(from2)) if !Type[To].isOption =>
          import from2.Underlying
          DerivationResult
            .direct { (await: DerivationResult.Await[TransformationExpr[To]]) =>
              // We're constructing:
              // ${ src }.map[partial.Result[$To]] { from2Expr: $from2 =>
              //   ${ derivedResultTo } // wrap if needed
              // }.getOrElse(partial.Result.empty)
              src
                .upcastExpr[Option[from2.Underlying]]
                .map(Expr.Function1.instance[from2.Underlying, partial.Result[To]] {
                  (from2Expr: Expr[from2.Underlying]) =>
                    await(deriveRecursiveTransformationExpr[from2.Underlying, To](from2Expr)).ensurePartial
                })
                .getOrElse(ChimneyExpr.PartialResult.fromEmpty[To])
            }
            .flatMap(DerivationResult.expandedPartial(_))
        case _ => DerivationResult.attemptNextRule
      }
  }
}
