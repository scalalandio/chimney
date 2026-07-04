package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}

import scala.quoted.*

final class TransformerDefinitionMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object TransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFieldConst[From, To, Overrides, Flags](td, selector, value)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFieldComputed[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
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
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFieldComputedFrom[From, To, Overrides, Flags](td, selectorFrom, selectorTo, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFieldRenamed[From, To, Overrides, Flags](td, selectorFrom, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFieldUnused[From, To, Overrides, Flags](td, selectorFrom)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withSealedSubtypeHandled[From, To, Overrides, Flags](td, f, m.typeOf_??[Subtype])
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromSubtype: Type,
      ToSubtype: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](td, m.typeOf_??[FromSubtype], m.typeOf_??[ToSubtype])
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeUnmatchedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](td, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFallback[From, To, Overrides, Flags](td, m.typeOf_??[FromFallback], fallback)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withFallbackFrom[From, To, Overrides, Flags](td, m.typeOf_??[FromFallback], selectorFrom, fallback)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withConstructor[From, To, Overrides, Flags](td, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.TransformerDefinitionDsl
      .withConstructorTo[From, To, Overrides, Flags](td, selector, f)
      .value
      .asInstanceOf[Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]]]
  }

  def withSourceFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.transformerDefinitionWithSourceFlag[From, To, Overrides, Flags](td, selectorFrom)
      .value
      .asInstanceOf[Expr[TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]]]
  }

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] = {
    val m = new TransformerDefinitionMacros(quotes)
    m.transformerDefinitionWithTargetFlag[From, To, Overrides, Flags](td, selectorTo)
      .value
      .asInstanceOf[Expr[TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]]]
  }
}
