package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerOverrides

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait TransformerDefinitionCommons[UpdateOverrides[_ <: TransformerOverrides]] {

  import TransformerDefinitionCommons.*

  /** Runtime storage for values and functions that Transformer definition is customized with. */
  val runtimeData: RuntimeDataStore
}
