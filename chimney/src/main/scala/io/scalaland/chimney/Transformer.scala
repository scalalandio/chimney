package io.scalaland.chimney

import scala.language.experimental.macros

import io.scalaland.chimney.internal.{ChimneyBlackboxMacros, Exported}

trait Transformer[From, To] {
  def transform(src: From): To
}

object Transformer extends LowPriorityTransformer {
  implicit def export[From, To]: Exported[Transformer[From, To]] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]
}

private[chimney] trait LowPriorityTransformer {
  implicit def importedTransformer[From, To](
    implicit exported: Exported[Transformer[From, To]]
  ): Transformer[From, To] = exported.instance
}
