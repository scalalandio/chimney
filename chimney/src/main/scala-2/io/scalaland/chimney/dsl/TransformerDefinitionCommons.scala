package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerOverrides

object TransformerDefinitionCommons {
  type RuntimeDataStore = io.scalaland.chimney.internal.runtime.RuntimeDataStore
  def emptyRuntimeDataStore: RuntimeDataStore = io.scalaland.chimney.internal.runtime.RuntimeDataStore.empty
}

private[chimney] trait TransformerDefinitionCommons[UpdateOverrides[_ <: TransformerOverrides]] {

  import TransformerDefinitionCommons.*

  /** Runtime storage for values and functions that Transformer definition is customized with. */
  val runtimeData: RuntimeDataStore
}
