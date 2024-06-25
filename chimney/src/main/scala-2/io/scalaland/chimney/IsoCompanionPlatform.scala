package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.iso.IsoMacros

import scala.language.experimental.macros

private[chimney] trait IsoCompanionPlatform { this: Iso.type =>

  /** Provides [[io.scalaland.chimney.Iso]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam First
    *   input type of the first conversion, output type of the second conversion
    * @tparam Second
    *   output type of the first conversion, input type of the second conversion
    * @return
    *   [[io.scalaland.chimney.Iso]] type class definition
    *
    * @since 1.2.0
    */
  def derive[First, Second]: Iso[First, Second] =
    macro IsoMacros.deriveIsoWithDefaults[First, Second]
}

private[chimney] trait IsoAutoDerivedCompanionPlatform { this: Iso.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.Iso.AutoDerived]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined), which is how it
    * differs from [[io.scalaland.chimney.auto#deriveAutomaticIso]].
    *
    * @tparam First
    *   input type of the first conversion, output type of the second conversion
    * @tparam Second
    *   output type of the first conversion, input type of the second conversion
    * @return
    *   [[io.scalaland.chimney.Iso.AutoDerived]] type class instance
    *
    * @since 1.2.0
    */
  implicit def deriveAutomatic[First, Second]: Iso.AutoDerived[First, Second] =
    macro IsoMacros.deriveIsoWithDefaults[First, Second]
}
