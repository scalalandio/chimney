package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait TransformerDefinitionCommons[UpdateCfg[_ <: TransformerCfg]] {

  import TransformerDefinitionCommons.*

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeDataStore

  /** updates runtime data in the upper transformer definition  */
  protected def __updateRuntimeData(newRuntimeData: RuntimeDataStore): this.type

  // used by generated code to help debugging

  /** Used internally by macro. Please don't use in your code. */
  final def __refineConfig[Cfg <: TransformerCfg]: UpdateCfg[Cfg] =
    this.asInstanceOf[UpdateCfg[Cfg]]

  /** Used internally by macro. Please don't use in your code. */
  final def __addOverride(overrideData: Any): this.type =
    __updateRuntimeData(overrideData +: runtimeData)

  /** Used internally by macro. Please don't use in your code. */
  final def __addInstance(instanceData: Any): this.type =
    __updateRuntimeData(instanceData +: runtimeData)
}
