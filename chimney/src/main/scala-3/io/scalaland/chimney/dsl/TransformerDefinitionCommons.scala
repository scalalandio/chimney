package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerCfg
import scala.annotation.static

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  @static final def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait TransformerDefinitionCommons[UpdateCfg[_ <: TransformerCfg]] {

  import TransformerDefinitionCommons.*

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeDataStore
}
