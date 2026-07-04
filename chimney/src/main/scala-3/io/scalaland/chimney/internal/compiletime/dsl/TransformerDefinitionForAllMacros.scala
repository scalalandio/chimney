package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.partial

import scala.quoted.*

final class TransformerDefinitionForAllMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object TransformerDefinitionForAllMacros {

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selectorFrom: Expr[FromMatch => T],
      selectorTo: Expr[ToMatch => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionForAllMacros(quotes)
    m.TransformerDefinitionForAllDsl
      .withFieldRenamed[From, To, Overrides, Flags, FromMatch, ToMatch](td, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionForAllMacros(quotes)
    m.TransformerDefinitionForAllDsl
      .withFieldConst[From, To, Overrides, Flags, FromMatch, ToMatch](td, selector, value)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionForAllMacros(quotes)
    m.TransformerDefinitionForAllDsl
      .withFieldComputed[From, To, Overrides, Flags, FromMatch, ToMatch](td, selector, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => partial.Result[U]]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionForAllMacros(quotes)
    m.TransformerDefinitionForAllDsl
      .withFieldComputedPartial[From, To, Overrides, Flags, FromMatch, ToMatch](td, selector, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }
}
