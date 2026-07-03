package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.PatcherUsing
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path}
import io.scalaland.chimney.internal.runtime.PatcherOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PatcherUsingMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

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
          weakTypeTag[PatcherUsing[A, Patch, Const[ObjPath, Overrides], Flags]]
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
          weakTypeTag[PatcherUsing[A, Patch, Computed[Path.Root, ObjPath, Overrides], Flags]]
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
          weakTypeTag[PatcherUsing[A, Patch, Computed[PatchPath, ObjPath, Overrides], Flags]]
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
          weakTypeTag[PatcherUsing[A, Patch, Ignored[PatchPath, Overrides], Flags]]
      }.applyFromSelector(selectorPatch)
    )

  def withPatchedValueFlagImpl[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: PatcherOverrides: WeakTypeTag,
      Flags <: PatcherFlags: WeakTypeTag
  ](selectorObj: Tree): Tree = {
    val pathObj = new ApplyFieldNameType {
      def apply[ObjPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] = weakTypeTag[ObjPath]
    }.applyFromSelector(selectorObj)
    q"""new _root_.io.scalaland.chimney.dsl.PatcherPatchedValueFlagsDsl.OfPatcherUsing[${weakTypeOf[
        A
      ]}, ${weakTypeOf[Patch]}, ${weakTypeOf[Overrides]}, ${weakTypeOf[Flags]}, $pathObj](${c.prefix.tree})"""
  }
}
