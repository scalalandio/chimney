package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class TransformerIntoMacros(q: Quotes) extends DslDefinitionsPlatform(q) with TransformIntoGateway

object TransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using quotes: Quotes): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerIntoMacros(quotes).withFieldConst(ti, selector, value)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using quotes: Quotes): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerIntoMacros(quotes).withFieldComputed(ti, selector, f)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using quotes: Quotes): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerIntoMacros(quotes).withFieldRenamed(ti, selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using quotes: Quotes): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerIntoMacros(quotes).withCoproductInstance(ti, f)
}
