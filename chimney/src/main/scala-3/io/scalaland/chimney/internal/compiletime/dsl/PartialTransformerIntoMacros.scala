package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.partial

import scala.quoted.*

final class PartialTransformerIntoMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object PartialTransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldConst[From, To, Overrides, Flags](ti, selector, value)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldConstPartial[From, To, Overrides, Flags](ti, selector, value)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputed[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
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
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputedFrom[From, To, Overrides, Flags](ti, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputedPartial[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputedPartialFrom[From, To, Overrides, Flags](ti, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[(From, Boolean) => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputedPartialFailFast[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialFromFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[(S, Boolean) => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldComputedPartialFromFailFast[From, To, Overrides, Flags](ti, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldRenamed[From, To, Overrides, Flags](ti, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFieldUnused[From, To, Overrides, Flags](ti, selectorFrom)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withSealedSubtypeHandled[From, To, Overrides, Flags](ti, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Subtype => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withSealedSubtypeHandledPartial[From, To, Overrides, Flags](ti, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[(Subtype, Boolean) => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withSealedSubtypeHandledPartialFailFast[From, To, Overrides, Flags](ti, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromSubtype: Type,
      ToSubtype: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](ti, m.typeOf_??[FromSubtype], m.typeOf_??[ToSubtype])
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeUnmatchedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](ti, selectorTo)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFallback[From, To, Overrides, Flags](ti, m.typeOf_??[FromFallback], fallback)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withFallbackFrom[From, To, Overrides, Flags](ti, m.typeOf_??[FromFallback], selectorFrom, fallback)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructor[From, To, Overrides, Flags](ti, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorTo[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorPartial[From, To, Overrides, Flags](ti, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorPartialTo[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorPartialFailFast[From, To, Overrides, Flags](ti, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialToFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorPartialToFailFast[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorEitherImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorEither[From, To, Overrides, Flags](ti, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorEitherToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.PartialTransformerIntoDsl
      .withConstructorEitherTo[From, To, Overrides, Flags](ti, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSourceFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.partialTransformerIntoWithSourceFlag[From, To, Overrides, Flags](ti, selectorFrom)
      .value
      .asInstanceOf[Expr[TransformerSourceFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, ? <: Path]]]
  }

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new PartialTransformerIntoMacros(quotes)
    m.partialTransformerIntoWithTargetFlag[From, To, Overrides, Flags](ti, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerTargetFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, ? <: Path]]]
  }
}
