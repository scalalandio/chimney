package io.scalaland.chimney.dsl

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros

export io.scalaland.chimney.inlined.{into, intoPartial, using}
export io.scalaland.chimney.syntax.{
  patchUsing,
  toPartialResult,
  toPartialResultOrString,
  transformInto,
  transformIntoPartial
}
