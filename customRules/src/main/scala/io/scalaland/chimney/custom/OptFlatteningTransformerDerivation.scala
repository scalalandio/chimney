package io.scalaland.chimney.custom

import io.scalaland.chimney.Transformer

import scala.language.experimental.macros

object OptFlatteningTransformerDerivation {
  implicit def derive[From, To]: Transformer[From, To] =
    macro OptFlatteningChimneyBlackboxMacros.deriveTransformerImpl[From, To]
}
