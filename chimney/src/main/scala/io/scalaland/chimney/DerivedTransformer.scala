package io.scalaland.chimney

import shapeless._

import io.scalaland.chimney.internal._

trait DerivedTransformer[From, To, Modifiers <: HList] {

  def transform(src: From, modifiers: Modifiers): To
}

object DerivedTransformer
    extends BasicInstances
    with ValueClassInstances
    with OptionInstances
    with EitherInstances
    with CollectionInstances
    with GenericInstances {

  final def apply[From, To, Modifiers <: HList](
    implicit dt: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[From, To, Modifiers] = dt

}

