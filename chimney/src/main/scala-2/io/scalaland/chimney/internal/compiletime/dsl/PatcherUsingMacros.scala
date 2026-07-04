package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PatcherUsingMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldConstImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, value: Tree)(@unused ev: Tree): Tree =
    PatcherUsingDsl
      .withFieldConst[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorObj), anyExpr(value))
      .toUntypedResult

  def withFieldComputedImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree =
    PatcherUsingDsl
      .withFieldComputed[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorObj), anyExpr(f))
      .toUntypedResult

  def withFieldComputedFromImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorPatch: Tree)(selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree =
    PatcherUsingDsl
      .withFieldComputedFrom[A, Patch, Overrides, Flags](
        prefixExpr,
        anyExpr(selectorPatch),
        anyExpr(selectorObj),
        anyExpr(f)
      )
      .toUntypedResult

  def withFieldIgnoredImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorPatch: Tree): Tree =
    PatcherUsingDsl
      .withFieldIgnored[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorPatch))
      .toUntypedResult

  def withPatchedValueFlagImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree): Tree =
    patcherUsingWithPatchedValueFlag[A, Patch, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.PatcherUsing[A, Patch, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorObj)
    ).toUntypedResult
}
