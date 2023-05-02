package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoImpl

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
    ${ TransformerIntoImpl.withFieldConstImpl('this, 'selector, 'value) }
  }

  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerIntoImpl.withFieldComputedImpl('this, 'selector, 'f) }
  }

  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerIntoImpl.withFieldRenamedImpl('this, 'selectorFrom, 'selectorTo) }
  }

  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] = {
    ${ TransformerIntoImpl.withCoproductInstanceImpl('this, 'f) }
  }

  inline def transform[ScopeFlags <: TransformerFlags](using tc: TransformerConfiguration[ScopeFlags]): To = {
    ${ TransformerIntoImpl.transform[From, To, Cfg, Flags, ScopeFlags]('source, 'td) }
  }
}
