package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerMacros
import scala.language.experimental.macros

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def gen[From, To]: Transformer[From, To] =
    macro TransformerMacros.genImpl[From, To]
}
