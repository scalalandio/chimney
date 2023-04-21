package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.dsl.*

/** Allows customization of [[io.scalaland.chimney.PartialTransformer]] derivation.
 *
 * @tparam From  type of input value
 * @tparam To    type of output value
 * @tparam Cfg   type-level encoded config
 * @tparam Flags type-level encoded flags
 *
 * @since 0.8.0
 */
final class PartialTransformerDefinition[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends FlagsDsl[[Flags1 <: TransformerFlags] =>> PartialTransformerDefinition[From, To, Cfg, Flags1], Flags]
    with TransformerDefinitionCommons[
      [Cfg1 <: TransformerCfg] =>> PartialTransformerDefinition[From, To, Cfg1, Flags]
    ] {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withFieldConstImpl('this, 'selector, 'value) }
  }

  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withFieldConstPartialImpl('this, 'selector, 'value) }
  }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withFieldComputedImpl('this, 'selector, 'f) }
  }

  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withFieldComputedPartialImpl('this, 'selector, 'f) }
  }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withFieldRenamed('this, 'selectorFrom, 'selectorTo) }
  }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withCoproductInstance('this, 'f) }
  }

  transparent inline def withCoproductInstancePartial[Inst](
      inline f: Inst => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] = {
    ${ PartialTransformerDefinitionImpl.withCoproductInstancePartial('this, 'f) }
  }

  inline def buildTransformer[ScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ScopeFlags]
  ): PartialTransformer[From, To] = {
    ${ PartialTransformerDefinitionImpl.buildTransformer[From, To, Cfg, Flags, ScopeFlags]('this) }
  }

  override protected def __updateRuntimeData(newRuntimeData: TransformerDefinitionCommons.RuntimeDataStore): this.type =
    new PartialTransformerDefinition(newRuntimeData).asInstanceOf[this.type]
}
