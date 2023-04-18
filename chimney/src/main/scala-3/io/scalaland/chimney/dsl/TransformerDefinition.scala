package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.*

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation.
 *
 * @tparam From  type of input value
 * @tparam To    type of output value
 * @tparam C     type-level encoded config
 * @tparam Flags type-level encoded flags
 *
 * @since 0.4.0
 */
final class TransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends FlagsDsl[[F1 <: TransformerFlags] =>> TransformerDefinition[From, To, C, F1], Flags]
    with TransformerDefinitionCommons[[C1 <: TransformerCfg] =>> TransformerDefinition[From, To, C1, Flags]] {

  // TODO: port macros

  override protected def __updateRuntimeData(newRuntimeData: TransformerDefinitionCommons.RuntimeDataStore): this.type =
    new TransformerDefinition(newRuntimeData).asInstanceOf[this.type]
}
