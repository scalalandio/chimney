package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.partial

final class PartialTransformerInto[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: PartialTransformerDefinition[From, To, Cfg, Flags]
) extends FlagsDsl[[Flags1 <: TransformerFlags] =>> PartialTransformerInto[From, To, Cfg, Flags1], Flags] {

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withFieldConst(selector, value))
  }

  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withFieldConstPartial(selector, value))
  }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withFieldComputed(selector, f))
  }

  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withFieldComputedPartial(selector, f))
  }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withFieldRenamed(selectorFrom, selectorTo))
  }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withCoproductInstance(f))
  }

  transparent inline def withCoproductInstancePartial[Inst](
      inline f: Inst => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new PartialTransformerInto(source, td.withCoproductInstancePartial(f))
  }

  inline def transform[ScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ScopeFlags]
  ): partial.Result[To] = {
    // TODO: rewrite to avoid instantiating a transformer by just inlining transformer body
    td.buildTransformer.transform(source)
  }

}
