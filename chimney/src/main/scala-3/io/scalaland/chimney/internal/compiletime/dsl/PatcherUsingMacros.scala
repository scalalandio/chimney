package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path}

import scala.quoted.*

final class PatcherUsingMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object PatcherUsingMacros {

  def withFieldConstImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      value: Expr[U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherUsingMacros(quotes)
    m.PatcherUsingDsl
      .withFieldConst[A, Patch, Overrides, Flags](pu, selectorObj, value)
      .value
      .asInstanceOf[Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withFieldComputedImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      f: Expr[Patch => U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherUsingMacros(quotes)
    m.PatcherUsingDsl
      .withFieldComputed[A, Patch, Overrides, Flags](pu, selectorObj, f)
      .value
      .asInstanceOf[Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]]]
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
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => S],
      selectorObj: Expr[A => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherUsingMacros(quotes)
    m.PatcherUsingDsl
      .withFieldComputedFrom[A, Patch, Overrides, Flags](pu, selectorPatch, selectorObj, f)
      .value
      .asInstanceOf[Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withFieldIgnoredImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => T]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] = {
    val m = new PatcherUsingMacros(quotes)
    m.PatcherUsingDsl
      .withFieldIgnored[A, Patch, Overrides, Flags](pu, selectorPatch)
      .value
      .asInstanceOf[Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]]]
  }

  def withPatchedValueFlagImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T]
  )(using Quotes): Expr[PatcherPatchedValueFlagsDsl.OfPatcherUsing[A, Patch, Overrides, Flags, ? <: Path]] = {
    val m = new PatcherUsingMacros(quotes)
    m.patcherUsingWithPatchedValueFlag[A, Patch, Overrides, Flags](pu, selectorObj)
      .value
      .asInstanceOf[Expr[PatcherPatchedValueFlagsDsl.OfPatcherUsing[A, Patch, Overrides, Flags, ? <: Path]]]
  }
}
