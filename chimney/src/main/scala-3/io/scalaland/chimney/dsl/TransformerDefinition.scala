package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.dsl.*

import scala.quoted.*

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation.
 *
 * @tparam From  type of input value
 * @tparam To    type of output value
 * @tparam Cfg   type-level encoded config
 * @tparam Flags type-level encoded flags
 *
 * @since 0.4.0
 */
final class TransformerDefinition[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends FlagsDsl[[Flags1 <: TransformerFlags] =>> TransformerDefinition[From, To, Cfg, Flags1], Flags]
    with TransformerDefinitionCommons[[Cfg1 <: TransformerCfg] =>> TransformerDefinition[From, To, Cfg1, Flags]] {

  /** Lifts current transformer definition as `PartialTransformer` definition
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    */
  def partial: PartialTransformerDefinition[From, To, Cfg, Flags] =
    new PartialTransformerDefinition[From, To, Cfg, Flags](runtimeData)

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerDefinitionImpl.withFieldConstImpl('this, 'selector, 'value) }
  }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerDefinitionImpl.withFieldComputedImpl('this, 'selector, 'f) }
  }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerDefinitionImpl.withFieldRenamed('this, 'selectorFrom, 'selectorTo) }
  }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerDefinitionImpl.withCoproductInstance('this, 'f) }
  }

  inline def buildTransformer[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Transformer[From, To] = {
    ${ TransformerDefinitionImpl.buildTransformer[From, To, Cfg, Flags, ImplicitScopeFlags]('this) }
  }

  override protected def __updateRuntimeData(newRuntimeData: TransformerDefinitionCommons.RuntimeDataStore): this.type =
    new TransformerDefinition(newRuntimeData).asInstanceOf[this.type]
}
