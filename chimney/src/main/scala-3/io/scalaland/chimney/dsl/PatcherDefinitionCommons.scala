package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherOverrides

object PatcherDefinitionCommons {
  type RuntimeDataStore = io.scalaland.chimney.internal.runtime.RuntimeDataStore
  final def emptyRuntimeDataStore: RuntimeDataStore = io.scalaland.chimney.internal.runtime.RuntimeDataStore.empty
}
private[chimney] trait PatcherDefinitionCommons[UpdateOverrides[_ <: PatcherOverrides]] {

  import PatcherDefinitionCommons.*

  /** Runtime storage for values and functions that Patcher definition is customized with. */
  val runtimeData: RuntimeDataStore
}
