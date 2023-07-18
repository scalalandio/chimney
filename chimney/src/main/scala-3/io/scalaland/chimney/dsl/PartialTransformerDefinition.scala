package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.compiletime.dsl.*
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

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
    ]
    with WithRuntimeDataStore {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstImpl('this, 'selector, 'value) }

  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstPartialImpl('this, 'selector, 'value) }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedImpl('this, 'selector, 'f) }

  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedPartialImpl('this, 'selector, 'f) }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldRenamed('this, 'selectorFrom, 'selectorTo) }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withCoproductInstance('this, 'f) }

  transparent inline def withCoproductInstancePartial[Inst](
      inline f: Inst => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withCoproductInstancePartial('this, 'f) }

  inline def buildTransformer[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): PartialTransformer[From, To] =
    ${ PartialTransformerDefinitionMacros.buildTransformer[From, To, Cfg, Flags, ImplicitScopeFlags]('this) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
