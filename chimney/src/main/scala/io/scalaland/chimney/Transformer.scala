package io.scalaland.chimney

import io.scalaland.chimney.internal.ChimneyMacros
import scala.language.experimental.macros

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyMacros.deriveTransformerImpl[From, To]
}
