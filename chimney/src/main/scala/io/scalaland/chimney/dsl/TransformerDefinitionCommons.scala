package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg

trait TransformerDefinitionCommons[UpdateCfg[_ <: TransformerCfg]] {

  val overrides: Map[String, Any]
  val instances: Map[(String, String), Any]

  protected def updated(newOverrides: Map[String, Any], newInstances: Map[(String, String), Any]): this.type

  // used by generated code to help debugging

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineConfig[C1 <: TransformerCfg]: UpdateCfg[C1] =
    this.asInstanceOf[UpdateCfg[C1]]

  /** Used internally by macro. Please don't use in your code.
    */
  def __addOverride(key: String, value: Any): this.type =
    updated(overrides.updated(key, value), instances)

  /** Used internally by macro. Please don't use in your code.
    */
  def __addInstance(from: String, to: String, value: Any): this.type =
    updated(overrides, instances.updated((from, to), value))
}
