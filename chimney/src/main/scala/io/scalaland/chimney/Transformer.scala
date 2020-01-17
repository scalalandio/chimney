package io.scalaland.chimney

import io.scalaland.chimney.internal.ChimneyBlackboxMacros
import io.scalaland.chimney.internal.dsl.TransformerDefinition

import scala.language.experimental.macros

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]

  def define[From, To]: TransformerDefinition[From, To, internal.Empty] =
    new TransformerDefinition[From, To, internal.Empty](Map.empty, Map.empty)
}
