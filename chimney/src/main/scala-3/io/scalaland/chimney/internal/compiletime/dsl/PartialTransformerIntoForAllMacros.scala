package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.partial

import scala.quoted.*

final class PartialTransformerIntoForAllMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object PartialTransformerIntoForAllMacros {

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
      ti: Expr[PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selectorFrom: Expr[FromMatch => T],
      selectorTo: Expr[ToMatch => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoForAllMacros(quotes)
    m.PartialTransformerIntoForAllDsl
      .withFieldRenamed[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoForAllMacros(quotes)
    m.PartialTransformerIntoForAllDsl
      .withFieldConst[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selector, value)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoForAllMacros(quotes)
    m.PartialTransformerIntoForAllDsl
      .withFieldComputed[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoForAllMacros(quotes)
    m.PartialTransformerIntoForAllDsl
      .withFieldComputedPartial[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }
}
