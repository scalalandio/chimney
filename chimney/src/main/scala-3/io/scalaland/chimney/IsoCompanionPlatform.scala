package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.iso.IsoMacros

private[chimney] trait IsoCompanionPlatform { this: Iso.type =>

  /** Provides [[io.scalaland.chimney.Iso]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.Iso]] type class definition
    *
    * @since 1.2.0
    */
  inline def derive[From, To]: Iso[From, To] =
    ${ IsoMacros.deriveIsoWithDefaults[From, To] }
}

private[chimney] trait IsoAutoDerivedCompanionPlatform { this: Iso.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.Iso.AutoDerived]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined), which is how it
    * differs from [[io.scalaland.chimney.auto#deriveAutomaticPartialTransformer]].
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.Iso.AutoDerived]] type class instance
    *
    * @since 1.2.0
    */
  implicit inline def deriveAutomatic[From, To]: Iso.AutoDerived[From, To] =
    ${ IsoMacros.deriveIsoWithDefaults[From, To] }
}
