package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerOverrides

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait TransformerDefinitionCommons[UpdateTail[_ <: TransformerOverrides]] {

  import TransformerDefinitionCommons.*

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeDataStore
}
