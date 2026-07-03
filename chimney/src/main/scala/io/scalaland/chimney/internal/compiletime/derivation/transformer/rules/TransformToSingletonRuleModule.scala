package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToSingletonRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformToSingletonRule extends Rule("ToSingleton") {

    private lazy val UnitType: Type[Unit] = Type.of[Unit]
    private lazy val NullType: Type[Null] = Type.of[Null]
    private lazy val NoneType: Type[None.type] = Type.of[None.type]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case _ if !(Type[To] =:= NullType) && (Type[To] <:< UnitType || Type[To] <:< NoneType) =>
          DerivationResult.attemptNextRuleBecause(
            s"Explicitly ignoring singletons of ${Type.prettyPrint[To]} due to safety concerns"
          )
        case Singleton(toExpr) => DerivationResult.expandedTotal(toExpr)
        case _                 => DerivationResult.attemptNextRule
      }
  }
}
