package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path}

import scala.quoted.*

final class PatcherDefinitionMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object PatcherDefinitionMacros {

  def withFieldConstImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pd: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      value: Expr[U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherDefinitionMacros(quotes)
    m.PatcherDefinitionDsl
      .withFieldConst[A, Patch, Overrides, Flags](pd, selectorObj, value)
      .value
      .asInstanceOf[Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pd: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      f: Expr[Patch => U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherDefinitionMacros(quotes)
    m.PatcherDefinitionDsl
      .withFieldComputed[A, Patch, Overrides, Flags](pd, selectorObj, f)
      .value
      .asInstanceOf[Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withFieldComputedFromImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      pd: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => S],
      selectorObj: Expr[A => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherDefinitionMacros(quotes)
    m.PatcherDefinitionDsl
      .withFieldComputedFrom[A, Patch, Overrides, Flags](pd, selectorPatch, selectorObj, f)
      .value
      .asInstanceOf[Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withFieldIgnoredImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pd: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => T]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherDefinitionMacros(quotes)
    m.PatcherDefinitionDsl
      .withFieldIgnored[A, Patch, Overrides, Flags](pd, selectorPatch)
      .value
      .asInstanceOf[Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withPatchedValueFlagImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pd: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T]
  )(using Quotes): Expr[PatcherPatchedValueFlagsDsl.OfPatcherDefinition[A, Patch, Overrides, Flags, ? <: Path]] = {
    val m = new PatcherDefinitionMacros(quotes)
    m.patcherDefinitionWithPatchedValueFlag[A, Patch, Overrides, Flags](pd, selectorObj)
      .value
      .asInstanceOf[Expr[PatcherPatchedValueFlagsDsl.OfPatcherDefinition[A, Patch, Overrides, Flags, ? <: Path]]]
  }
}
