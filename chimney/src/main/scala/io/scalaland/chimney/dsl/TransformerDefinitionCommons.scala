package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[dsl] trait TransformerDefinitionCommons[UpdateCfg[_ <: TransformerCfg]] {

  import TransformerDefinitionCommons.*

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeDataStore

  /** updates runtime data in the upper transformer definition  */
  protected def __updateRuntimeData(newRuntimeData: RuntimeDataStore): this.type

  // used by generated code to help debugging

  /** Used internally by macro. Please don't use in your code. */
  def __refineConfig[C1 <: TransformerCfg]: UpdateCfg[C1] =
    this.asInstanceOf[UpdateCfg[C1]]

  /** Used internally by macro. Please don't use in your code. */
  def __addOverride(overrideData: Any): this.type =
    __updateRuntimeData(overrideData +: runtimeData)

  /** Used internally by macro. Please don't use in your code. */
  def __addInstance(instanceData: Any): this.type =
    __updateRuntimeData(instanceData +: runtimeData)
}
