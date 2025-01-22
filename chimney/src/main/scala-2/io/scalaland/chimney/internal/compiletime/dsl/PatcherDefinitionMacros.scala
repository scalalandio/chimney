package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.PatcherDefinition
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path}
import io.scalaland.chimney.internal.runtime.PatcherOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PatcherDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldConstImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ObjPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PatcherDefinition[A, Patch, Const[ObjPath, Overrides], Flags]]
      }.applyFromSelector(selectorObj)
    )

  def withFieldComputedImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ObjPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PatcherDefinition[A, Patch, Computed[Path.Root, ObjPath, Overrides], Flags]]
      }.applyFromSelector(selectorObj)
    )

  def withFieldComputedFromImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorPatch: Tree)(selectorObj: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[PatchPath <: Path: WeakTypeTag, ObjPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PatcherDefinition[A, Patch, Computed[PatchPath, ObjPath, Overrides], Flags]]
      }.applyFromSelectors(selectorPatch, selectorObj)
    )

  def withFieldIgnoredImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorPatch: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[PatchPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PatcherDefinition[A, Patch, Ignored[PatchPath, Overrides], Flags]]
      }.applyFromSelector(selectorPatch)
    )
}
