package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.{Log, MIO}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToOptionRuleModule {
  this: Derivation & TransformOptionToOptionRuleModule & hearth.MacroCommons =>

  // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

  private def optionTypeCompat[A: Type]: Type[Option[A]] = Type.of[Option[A]]

  private def optionExprCompat[A: Type](value: Expr[A]): Expr[Option[A]] = Expr.quote {
    scala.Option[A](Expr.splice(value))
  }

  private def wrapFallbackOptionCompat[A: Type](value: Expr[A]): ExistentialExpr = {
    implicit val OptionAType: Type[Option[A]] = optionTypeCompat[A]
    optionExprCompat(value).as_??
  }

  protected object TransformToOptionRule extends Rule("ToOption") {

    private lazy val NoneType: Type[None.type] = Type.of[None.type]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[To] match {
        case _ if Type[To] <:< NoneType =>
          notSupportedTransformerDerivation(ctx)
            .logInfo(s"Discovered that target type is ${Type.prettyPrint(using NoneType)} which we explicitly reject")
        case OptionalValue(_) =>
          Log.namedScope("Lifting transformation into Option") {
            // $COVERAGE-OFF$scope detail is only built when Info logging is rendered (off by default, incl. in tests)
            Log.info(
              s"Lifting ${Type.prettyPrint[From]} -> ${Type
                  .prettyPrint[To]} transformation into ${Type.prettyPrint(using optionTypeCompat[From])} -> ${Type.prettyPrint[To]}"
            ) >>
              // $COVERAGE-ON$
              wrapInOptionAndTransform[From, To]
          }
        case _ =>
          attemptNextRule
      }
  }

  private def wrapInOptionAndTransform[From, To](implicit
      ctx: TransformationContext[From, To]
  ): MIO[Rule.ExpansionResult[To]] = {
    implicit val OptionFromType: Type[Option[From]] = optionTypeCompat[From]
    // We're constructing:
    // '{ ${ derivedTo2 } /* created from Option(src) */  }
    TransformOptionToOptionRule.expand(
      ctx.updateFromTo[Option[From], To](optionExprCompat(ctx.src), updateFallbacks = wrapFallbacks)
    )
  }

  private val wrapFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = {
    case fb @ TransformerOverride.Fallback(fallback) =>
      import fallback.{Underlying as Fallback, value as fallbackExpr}
      Vector(Type[Fallback] match {
        case OptionalValue(_) => fb
        case _                => TransformerOverride.Fallback(wrapFallbackOptionCompat(fallbackExpr))
      })
  }
}
