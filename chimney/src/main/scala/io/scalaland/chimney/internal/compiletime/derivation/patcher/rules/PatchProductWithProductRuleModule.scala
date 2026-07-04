package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.{DerivationError, FailedPolicyCheck, PatchFieldNotFoundInTargetObj}
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation
import io.scalaland.chimney.internal.compiletime.derivation.transformer.rules.TransformProductToProductRuleModule

private[compiletime] trait PatchProductWithProductRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  protected object PatchProductWithProductRule extends Rule("PatchProductWithProduct") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      TransformProductToProductRule.expand[Patch, A].recoverWith { errors =>
        ctx match {
          case Patched(_) =>
            val head +: tail = errors.toVector.map(DerivationError.fromThrowable).flatMap(mapErrors[A]): @unchecked
            MIO.fail(head, tail*)
          case _ => MIO.fail(errors)
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
