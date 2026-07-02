package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.iso.IsoMacros

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
  inline def derive[First, Second]: Iso[First, Second] =
    ${ IsoMacros.deriveIsoWithDefaults[First, Second] }
}
