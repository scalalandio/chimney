package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From]) match {
        case (_: TransformationContext.ForPartial[?, ?], Type.Option(from2)) if !(Type[To] <:< Type[Option[Any]]) =>
          ComputedType.use(from2) { implicit InnerFrom: Type[from2.Underlying] =>
            // We're constructing:
            // ${ src }.map[partial.Result[$To]] { from2: $from2 =>
            //   ${ derivedResultTo } // wrap if needed
            // }.getOrElse(partial.Result.empty)
            DerivationResult
              .direct { (await: DerivationResult.Await[TransformationExpr[To]]) =>
                Expr.Option.getOrElse(
                  Expr.Option.map[from2.Underlying, partial.Result[To]](ctx.src.upcastExpr[Option[from2.Underlying]])(
                    Expr.Function1.lift[from2.Underlying, partial.Result[To]] { (param: Expr[from2.Underlying]) =>
                      await(deriveRecursiveTransformationExpr[from2.Underlying, To](param)).ensurePartial
                    }
                  )
                )(ChimneyExpr.PartialResult.fromEmpty[To])
              }
              .flatMap(DerivationResult.partialExpr(_))
          }
        case _ => DerivationResult.continue
      }
  }
}
