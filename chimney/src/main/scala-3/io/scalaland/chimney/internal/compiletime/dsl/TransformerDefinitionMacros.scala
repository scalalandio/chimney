package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class TransformerDefinitionMacros(q: Quotes) extends DslDefinitionsPlatform(q) with TransformerDefinitionGateway

object TransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using quotes: Quotes): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerDefinitionMacros(quotes).withFieldConst(td, selector, value)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using quotes: Quotes): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerDefinitionMacros(quotes).withFieldComputed(td, selector, f)

  def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using quotes: Quotes): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerDefinitionMacros(quotes).withFieldRenamed(td, selectorFrom, selectorTo)

  def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using quotes: Quotes): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    new TransformerDefinitionMacros(quotes).withCoproductInstance(td, f)
}
