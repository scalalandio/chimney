package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.codec.CodecMacros

import scala.language.experimental.macros

private[chimney] trait CodecCompanionPlatform { this: Codec.type =>

  /** Provides [[io.scalaland.chimney.Codec]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam Domain
    *   type of domain value
    * @tparam Dto
    *   type of DTO value
    * @return
    *   [[io.scalaland.chimney.Codec]] type class definition
    *
    * @since 1.2.0
    */
  def derive[Domain, Dto]: Codec[Domain, Dto] =
    macro CodecMacros.deriveCodecWithDefaults[Domain, Dto]
}

private[chimney] trait CodecAutoDerivedCompanionPlatform { this: Codec.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.Codec.AutoDerived]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined), which is how it
    * differs from [[io.scalaland.chimney.auto#deriveAutomaticCodec]].
    *
    * @tparam Domain
    *   type of domain value
    * @tparam Dto
    *   type of DTO value
    * @return
    *   [[io.scalaland.chimney.Codec.AutoDerived]] type class instance
    *
    * @since 1.2.0
    */
  implicit def deriveAutomatic[Domain, Dto]: Codec.AutoDerived[Domain, Dto] =
    macro CodecMacros.deriveCodecWithDefaults[Domain, Dto]
}
