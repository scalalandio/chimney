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
