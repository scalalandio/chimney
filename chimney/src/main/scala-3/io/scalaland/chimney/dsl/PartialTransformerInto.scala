package io.scalaland.chimney.dsl

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerIntoMacros
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}
import io.scalaland.chimney.partial

final class PartialTransformerInto[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: PartialTransformerDefinition[From, To, Cfg, Flags]
) extends FlagsDsl[[Flags1 <: TransformerFlags] =>> PartialTransformerInto[From, To, Cfg, Flags1], Flags]
    with WithRuntimeDataStore {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withFieldConstImpl('this, 'selector, 'value) }

  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withFieldConstPartialImpl('this, 'selector, 'value) }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withFieldComputedImpl('this, 'selector, 'f) }

  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withFieldComputedPartialImpl('this, 'selector, 'f) }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withFieldRenamedImpl('this, 'selectorFrom, 'selectorTo) }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withCoproductInstanceImpl('this, 'f) }

  transparent inline def withCoproductInstancePartial[Inst](
      inline f: Inst => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerIntoMacros.withCoproductInstancePartialImpl('this, 'f) }

  inline def transform[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    ${
      TransformerMacros.derivePartialTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](
        'source,
        'td,
        failFast = false
      )
    }

  inline def transformFailFast[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    ${
      TransformerMacros.derivePartialTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](
        'source,
        'td,
        failFast = true
      )
    }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerInto(source = source, td = td.addOverride(overrideData)).asInstanceOf[this.type]
}
