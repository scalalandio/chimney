package io.scalaland.chimney

import shapeless._

import io.scalaland.chimney.internal._

trait DerivedTransformer[-From, To, Modifiers <: HList] {

  def transform(src: From, modifiers: Modifiers): To
}

object DerivedTransformer extends DerivedTransformerInstances {

  final def apply[From, To, Modifiers <: HList](
    implicit dt: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[From, To, Modifiers] = dt

  implicit final def fromTransformer[T, U, Modifiers <: HList](
    implicit transformer: Transformer[T, U]
  ): DerivedTransformer[T, U, Modifiers] =
    (src: T, _: Modifiers) => transformer.transform(src)
}
