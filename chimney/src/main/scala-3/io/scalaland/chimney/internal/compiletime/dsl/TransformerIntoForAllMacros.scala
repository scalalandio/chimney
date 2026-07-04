package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}

import scala.quoted.*

final class TransformerIntoForAllMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object TransformerIntoForAllMacros {

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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selectorFrom: Expr[FromMatch => T],
      selectorTo: Expr[ToMatch => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoForAllMacros(quotes)
    m.TransformerIntoForAllDsl
      .withFieldRenamed[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoForAllMacros(quotes)
    m.TransformerIntoForAllDsl
      .withFieldConst[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selector, value)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoForAllMacros(quotes)
    m.TransformerIntoForAllDsl
      .withFieldComputed[From, To, Overrides, Flags, FromMatch, ToMatch](ti, selector, f)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }
}
