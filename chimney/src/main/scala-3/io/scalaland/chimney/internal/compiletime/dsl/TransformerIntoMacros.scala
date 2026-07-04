package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}

import scala.quoted.*

final class TransformerIntoMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object TransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFieldConst[From, To, Overrides, Flags](ti, selector, value)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFieldComputed[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFieldComputedFrom[From, To, Overrides, Flags](ti, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFieldRenamed[From, To, Overrides, Flags](ti, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFieldUnused[From, To, Overrides, Flags](ti, selectorFrom)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withSealedSubtypeHandled[From, To, Overrides, Flags](ti, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromSubtype: Type,
      ToSubtype: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](ti, m.typeOf_??[FromSubtype], m.typeOf_??[ToSubtype])
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeUnmatchedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](ti, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFallback[From, To, Overrides, Flags](ti, m.typeOf_??[FromFallback], fallback)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withFallbackFrom[From, To, Overrides, Flags](ti, m.typeOf_??[FromFallback], selectorFrom, fallback)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withConstructor[From, To, Overrides, Flags](ti, f)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerIntoMacros(quotes)
    m.TransformerIntoDsl
      .withConstructorTo[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSourceFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new TransformerIntoMacros(quotes)
    m.transformerIntoWithSourceFlag[From, To, Overrides, Flags](ti, selectorFrom)
      .value
      .asInstanceOf[Expr[TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]]]
  }

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new TransformerIntoMacros(quotes)
    m.transformerIntoWithTargetFlag[From, To, Overrides, Flags](ti, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]]]
  }
}
