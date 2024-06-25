package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

import scala.annotation.targetName

private[chimney] trait PartialTransformerCompanionPlatform extends PartialTransformerLowPriorityImplicits1 {
  this: PartialTransformer.type =>

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.PartialTransformer]] type class definition
    *
    * @since 0.8.0
    */
  inline def derive[From, To]: PartialTransformer[From, To] =
    ${ TransformerMacros.derivePartialTransformerWithDefaults[From, To] }
}

private[chimney] trait PartialTransformerAutoDerivedCompanionPlatform { this: PartialTransformer.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.PartialTransformer.AutoDerived]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined), which is how it
    * differs from [[io.scalaland.chimney.auto#deriveAutomaticPartialTransformer]].
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.PartialTransformer.AutoDerived]] type class instance
    *
    * @since 0.8.0
    */
  @targetName("derive") // all methods were suppose to be named deriveAutomatic, but this one slipped through
  implicit inline def deriveAutomatic[From, To]: PartialTransformer.AutoDerived[From, To] =
    ${ TransformerMacros.derivePartialTransformerWithDefaults[From, To] }
}
