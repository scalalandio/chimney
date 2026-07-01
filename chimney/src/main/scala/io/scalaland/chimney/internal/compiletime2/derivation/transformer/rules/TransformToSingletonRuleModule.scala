package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformToSingletonRuleModule` - 1:1 copy
  * (`Type[Unit]`/`Type[Null]`/`Type[None.type]` instances come from local `Type.of` lazies instead of
  * `Type.Implicits`).
  */
private[compiletime2] trait TransformToSingletonRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformToSingletonRule extends Rule("ToSingleton") {

    private lazy val UnitType: Type[Unit] = Type.of[Unit]
    private lazy val NullType: Type[Null] = Type.of[Null]
    private lazy val NoneType: Type[None.type] = ScalaType.Option.None

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
