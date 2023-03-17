package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.*

final class TransformerInto[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, Cfg, Flags]
) extends FlagsDsl[[Flags1 <: TransformerFlags] =>> TransformerInto[From, To, Cfg, Flags1], Flags] {

  def partial: PartialTransformerInto[From, To, Cfg, Flags] =
    new PartialTransformerInto[From, To, Cfg, Flags](source, td.partial)

  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new TransformerInto(source, td.withFieldConst(selector, value))
  }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new TransformerInto(source, td.withFieldComputed(selector, f))
  }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new TransformerInto(source, td.withFieldRenamed(selectorFrom, selectorTo))
  }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    new TransformerInto(source, td.withCoproductInstance(f))
  }

  inline def transform[ScopeFlags <: TransformerFlags](using tc: TransformerConfiguration[ScopeFlags]): To = {
    // TODO: rewrite to avoid instantiating a transformer by just inlining transformer body
    td.buildTransformer.transform(source)
  }
}
