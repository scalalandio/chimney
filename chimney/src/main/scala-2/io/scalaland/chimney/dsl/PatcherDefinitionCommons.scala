package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherOverrides

object PatcherDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait PatcherDefinitionCommons[UpdateOverrides[_ <: PatcherOverrides]] {

  import PatcherDefinitionCommons.*

  /** Runtime storage for values and functions that Patcher definition is customized with. */
  val runtimeData: RuntimeDataStore
}
