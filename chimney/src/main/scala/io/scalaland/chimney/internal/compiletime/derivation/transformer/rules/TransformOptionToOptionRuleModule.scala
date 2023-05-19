package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformOptionToOptionRuleModule { this: Derivation =>

  object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Option(innerFrom), Type.Option(innerTo)) =>
          ComputedType.use(innerFrom) { implicit InnerFrom: Type[innerFrom.Underlying] =>
            ComputedType.use(innerTo) { implicit InnerTo: Type[innerTo.Underlying] =>
              deriveFold[innerFrom.Underlying, innerTo.Underlying, To] { (newFromExpr: Expr[innerFrom.Underlying]) =>
                deriveRecursiveTransformationExpr[innerFrom.Underlying, innerTo.Underlying](newFromExpr)
                  .map(Rule.ExpansionResult.Expanded(_))
              } { provideFromTotal =>
                // TODO:
                // sth like: DerivedExpr.Total('{ ${ ctx.src }.map(from => ${ provideFromTotal('{ from }) }) })
                DerivedExpr.TotalExpr(Expr.asInstanceOf[Nothing, To](Expr.Nothing)(Type.Nothing, Type[To]))
              } { provideFromPartial =>
                // TODO:
                // sth like: DerivedExpr.Partial('{
                //   ${ src.src }.fold[$To](
                //      partial.Result.Value(None : Option[$To])
                //   )(
                //     (from: $InnerFrom) => ${ provideFromPartial('{ from }) }.map(Option[$InnerTo](_))
                //   )
                // })
                DerivedExpr.TotalExpr(Expr.asInstanceOf[Nothing, To](Expr.Nothing)(Type.Nothing, Type[To]))
              }
            }
          }
        case _ =>
          DerivationResult.continue
      }
  }
}
