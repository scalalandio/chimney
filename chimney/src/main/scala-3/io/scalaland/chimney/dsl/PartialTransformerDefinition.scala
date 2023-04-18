package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.*

/** Allows customization of [[io.scalaland.chimney.PartialTransformer]] derivation.
 *
 * @tparam From  type of input value
 * @tparam To    type of output value
 * @tparam C     type-level encoded config
 * @tparam Flags type-level encoded flags
 *
 * @since 0.8.0
 */
final class PartialTransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends FlagsDsl[[F1 <: TransformerFlags] =>> PartialTransformerDefinition[From, To, C, F1], Flags]
    with TransformerDefinitionCommons[
      [C1 <: TransformerCfg] =>> PartialTransformerDefinition[From, To, C1, Flags],
    ] {

  // TODO: port macros

  override protected def __updateRuntimeData(newRuntimeData: TransformerDefinitionCommons.RuntimeDataStore): this.type =
    new PartialTransformerDefinition(newRuntimeData).asInstanceOf[this.type]
}
