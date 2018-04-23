package io.scalaland.chimney

import io.scalaland.chimney.internal.ChimneyBlackboxMacros
import scala.language.experimental.macros

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]
}
