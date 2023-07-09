package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class PartialTransformerIntoMacros(q: Quotes) extends DslDefinitionsPlatform(q) with PartialTransformerIntoGateway

object PartialTransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withFieldConst(pti, selector, value)

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withFieldConstPartial(pti, selector, value)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withFieldComputed(pti, selector, f)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withFieldComputedPartial(pti, selector, f)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withFieldRenamed(pti, selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withCoproductInstance(pti, f)

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      pti: Expr[dsls.PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => partial.Result[To]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerIntoMacros(quotes).withCoproductInstancePartial(pti, f)
}
