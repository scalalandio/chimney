package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.partial

import scala.quoted.*

final class PartialTransformerDefinitionMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object PartialTransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldConst[From, To, Overrides, Flags](td, selector, value)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldConstPartial[From, To, Overrides, Flags](td, selector, value)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputed[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputedFrom[From, To, Overrides, Flags](td, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputedPartial[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputedPartialFrom[From, To, Overrides, Flags](td, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[(From, Boolean) => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputedPartialFailFast[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[(S, Boolean) => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldComputedPartialFromFailFast[From, To, Overrides, Flags](td, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldRenamed[From, To, Overrides, Flags](td, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFieldUnused[From, To, Overrides, Flags](td, selectorFrom)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withSealedSubtypeHandled[From, To, Overrides, Flags](td, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withSealedSubtypeHandledPartial[From, To, Overrides, Flags](td, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[(Subtype, Boolean) => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withSealedSubtypeHandledPartialFailFast[From, To, Overrides, Flags](td, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromSubtype: Type,
      ToSubtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](td, m.typeOf_??[FromSubtype], m.typeOf_??[ToSubtype])
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeUnmatchedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](td, selectorTo)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFallback[From, To, Overrides, Flags](td, m.typeOf_??[FromFallback], fallback)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withFallbackFrom[From, To, Overrides, Flags](td, m.typeOf_??[FromFallback], selectorFrom, fallback)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructor[From, To, Overrides, Flags](td, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorTo[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorPartial[From, To, Overrides, Flags](td, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorPartialTo[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorPartialFailFast[From, To, Overrides, Flags](td, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorPartialToFailFastImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorPartialToFailFast[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorEitherImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorEither[From, To, Overrides, Flags](td, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorEitherToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.PartialTransformerDefinitionDsl
      .withConstructorEitherTo[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSourceFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using
      Quotes
  ): Expr[TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.partialTransformerDefinitionWithSourceFlag[From, To, Overrides, Flags](td, selectorFrom)
      .value
      .asInstanceOf[Expr[
        TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]
      ]]
  }

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using
      Quotes
  ): Expr[TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new PartialTransformerDefinitionMacros(quotes)
    m.partialTransformerDefinitionWithTargetFlag[From, To, Overrides, Flags](td, selectorTo)
      .value
      .asInstanceOf[Expr[
        TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]
      ]]
  }
}
