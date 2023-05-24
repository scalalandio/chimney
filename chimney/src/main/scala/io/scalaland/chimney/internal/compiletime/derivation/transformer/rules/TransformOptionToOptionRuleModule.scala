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

              val optionArgFT = newFreshIdent[innerFrom.Underlying]("optionInner")
              // generates new name like optionInner$1234

              deriveRecursiveTransformationExpr[innerFrom.Underlying, innerTo.Underlying](
                newSrc = optionArgFT.asIdentExpr[innerFrom.Underlying] // reference to optionInner$1234 as expr
              ).map {
                case DerivedExpr.TotalExpr(innerTransformExpr) => // innerTransformExpr may contain references to optionInner$1234, passed as newSrc
                  DerivedExpr.TotalExpr(
                    ctx.srcExpr // Expr[Option[From]]
                      .mapOptionExpr( // needs Expr[From] => Expr[To]
                        optionArgFT.asFunctionExpr(
                          innerTransformExpr // needs Expr[To]
                        ) // returns Expr[From] => Expr[To], equivalent to '{ optionInner$1234 => $innerTransformExpr }
                      ) // Expr[Option[To]]
                  ) // TotalExpr

                case DerivedExpr.PartialExpr(innerPartialTransformExpr) =>
                  DerivedExpr.PartialExpr(
                    ctx.srcExpr // Expr[Option[From]]
                      .foldOptionExpr( // needs 2 exprs:
                        ifEmptyExpr = ChimneyExpr.PartialResult.Value[Option[To]](Expr.Option.empty[To]),
                        fExpr = // Expr[From] => Expr[partial.Result[Option[To]]]
                          optionArgFT.asFunctionExpr(
                            innerPartialTransformExpr
                              .partialMapExpr(Exprs.Option.apply) // Expr[partial.Result[Option[To]]]
                          ) // Expr[From] => Expr[partial.Result[Option[To]]]
                      ) // Expr[partial.Result[Option[To]]]
                  ) // PartialExpr
              }.map(Rule.ExpansionResult.Expanded(_))

//              deriveFold[innerFrom.Underlying, innerTo.Underlying, To] { (newFromExpr: Expr[innerFrom.Underlying]) =>
//                deriveRecursiveTransformationExpr[innerFrom.Underlying, innerTo.Underlying](newFromExpr)
//                  .map(Rule.ExpansionResult.Expanded(_))
//              } { provideFromTotal =>
//                // TODO:
//                // sth like: DerivedExpr.Total('{ ${ ctx.src }.map(from => ${ provideFromTotal('{ from }) }) })
//                DerivedExpr.TotalExpr(Expr.asInstanceOf[Nothing, To](Expr.Nothing)(Type.Nothing, Type[To]))
//              } { provideFromPartial =>
//                // TODO:
//                // sth like: DerivedExpr.Partial('{
//                //   ${ src.src }.fold[$To](
//                //      partial.Result.Value(None : Option[$To])
//                //   )(
//                //     (from: $InnerFrom) => ${ provideFromPartial('{ from }) }.map(Option[$InnerTo](_))
//                //   )
//                // })
//                DerivedExpr.TotalExpr(Expr.asInstanceOf[Nothing, To](Expr.Nothing)(Type.Nothing, Type[To]))
//              }
            }
          }
        case _ =>
          DerivationResult.continue
      }
  }
}
