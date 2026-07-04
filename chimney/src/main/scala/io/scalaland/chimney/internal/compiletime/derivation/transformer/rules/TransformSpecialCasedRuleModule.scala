package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** Consults Chimney's engine-aware macro extensions (`io.scalaland.chimney.integrations.ChimneyMacroExtension`) for a
  * PAIR-SPECIFIC transformation of `(From, To)`.
  *
  * Slot: AFTER the four implicit rules (so user/integration implicits win) and BEFORE the built-in structural rules (so
  * a registered handler beats Chimney's default structural derivation). Loads handlers once per expansion, then follows
  * the owner's sketch:
  * {{{
  * (Type[From], Type[To]) match {
  *   case IsChimneySpecialCased(handler) => handler.specialCase(using ctx)
  *   case _                              => // yield to the next rule
  * }
  * }}}
  */
private[compiletime] trait TransformSpecialCasedRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformSpecialCasedRule extends Rule("SpecialCased") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] = {
      ensureChimneyMacroExtensionsLoaded()
      (Type[From], Type[To]) match {
        case IsChimneySpecialCased(handler) =>
          handler.specialCase.map {
            case Some(transformationExpr) => Rule.ExpansionResult.Expanded(transformationExpr)
            case None                     => Rule.ExpansionResult.AttemptNextRule(None)
          }
        case _ => attemptNextRule[To]
      }
    }
  }
}
