package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class PartialTransformerDefinitionMacros(q: Quotes)
    extends DslDefinitionsPlatform(q)
    with PartialTransformerDefinitionGateway

object PartialTransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withFieldConst(ptd, selector, value)

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withFieldConstPartial(ptd, selector, value)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withFieldComputed(ptd, selector, f)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withFieldComputedPartial(ptd, selector, f)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withFieldRenamed(ptd, selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withCoproductInstance(ptd, f)

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => partial.Result[To]]
  )(using quotes: Quotes): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new PartialTransformerDefinitionMacros(quotes).withCoproductInstancePartial(ptd, f)
}
