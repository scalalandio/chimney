package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]

  def define[From, To]: TransformerDefinition[From, To, TransformerCfg.Empty] =
    new TransformerDefinition[From, To, TransformerCfg.Empty](Map.empty, Map.empty)
}
