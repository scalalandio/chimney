package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.{
  DerivationError,
  DerivationErrors,
  DerivationResult,
  FailedPolicyCheck,
  PatchFieldNotFoundInTargetObj
}
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation
import io.scalaland.chimney.internal.compiletime.derivation.transformer.rules.TransformProductToProductRuleModule

/** Hearth-based port of `...compiletime.derivation.patcher.rules.PatchProductWithProductRuleModule` - 1:1 copy
  * (`errors.asVector` classifies the raw MIO `Throwable`s back into `DerivationError`s; `DerivationErrors(head, tail)`
  * becomes the vararg-splatted `DerivationErrors(head, tail*)`; MIO's `recoverWith` takes a `PartialFunction`, to which
  * the old total-lambda literal adapts with identical semantics).
  */
private[compiletime] trait PatchProductWithProductRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  protected object PatchProductWithProductRule extends Rule("PatchProductWithProduct") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      TransformProductToProductRule.expand[Patch, A].recoverWith { errors =>
        ctx match {
          case Patched(_) =>
            val head +: tail = errors.asVector.flatMap(mapErrors[A]): @unchecked
            DerivationResult.fail(DerivationErrors(head, tail*))
          case _ => DerivationResult.fail(errors)
        }
      }

    private def mapErrors[A: Type]: DerivationError => Vector[DerivationError] = {
      case DerivationError.TransformerError(FailedPolicyCheck(_, _, failedValues)) =>
        failedValues.toVector.map { field =>
          DerivationError.PatcherError(PatchFieldNotFoundInTargetObj(field, Type.prettyPrint[A]))
        }
      case error => Vector(error)
    }
  }
}
