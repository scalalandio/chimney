package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

import scala.language.experimental.macros

private[chimney] trait PartialTransformerCompanionPlatform { this: PartialTransformer.type =>

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class definition
    *
    * @since 0.7.0
    */
  def derive[From, To]: PartialTransformer[From, To] =
    macro TransformerMacros.derivePartialTransformerWithDefaults[From, To]
}

private[chimney] trait PartialTransformerAutoDerivedCompanionPlatform { this: PartialTransformer.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.PartialTransformer.AutoDerived]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined),
    * which is how it differs from [[io.scalaland.chimney.auto#deriveAutomaticPartialTransformer]].
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.PartialTransformer.AutoDerived]] type class instance
    *
    * @since 0.8.0
    */
  implicit def deriveAutomatic[From, To]: PartialTransformer.AutoDerived[From, To] =
    macro TransformerMacros.derivePartialTransformerWithDefaults[From, To]
}
