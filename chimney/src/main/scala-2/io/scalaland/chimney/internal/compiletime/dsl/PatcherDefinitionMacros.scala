package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PatcherDefinitionMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldConstImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, value: Tree)(@unused ev: Tree): Tree =
    PatcherDefinitionDsl
      .withFieldConst[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorObj), anyExpr(value))
      .toUntypedResult

  def withFieldComputedImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree =
    PatcherDefinitionDsl
      .withFieldComputed[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorObj), anyExpr(f))
      .toUntypedResult

  def withFieldComputedFromImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorPatch: Tree)(selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree =
    PatcherDefinitionDsl
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
    PatcherDefinitionDsl
      .withFieldIgnored[A, Patch, Overrides, Flags](prefixExpr, anyExpr(selectorPatch))
      .toUntypedResult

  def withPatchedValueFlagImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree): Tree =
    patcherDefinitionWithPatchedValueFlag[A, Patch, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.PatcherDefinition[A, Patch, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorObj)
    ).toUntypedResult
}
