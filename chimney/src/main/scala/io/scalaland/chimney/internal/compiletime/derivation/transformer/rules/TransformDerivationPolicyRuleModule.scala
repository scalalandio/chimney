package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformDerivationPolicyRuleModule { this: Derivation & hearth.MacroCommons =>

  /** Enforces the derivation policy (see
    * [[io.scalaland.chimney.internal.compiletime.derivation.DerivationPolicy DerivationPolicy]]) directly before the
    * structural rules (ProductToProduct / SealedHierarchyToSealedHierarchy / PatchProductWithProduct): any [From, To]
    * pair reaching this position fell through every non-structural rule, so gating here gates exactly "generating new
    * transformation code for a product/sum type". The check runs once per macro expansion - when the outermost
    * structural derivation is permitted, every nested one is too - and always yields to the next rule when permitted.
    */
  protected object TransformDerivationPolicyRule extends Rule("DerivationPolicy") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      checkDerivationPolicyOncePerExpansion(
        s"Chimney transformation from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
      ) >> attemptNextRule
  }
}
