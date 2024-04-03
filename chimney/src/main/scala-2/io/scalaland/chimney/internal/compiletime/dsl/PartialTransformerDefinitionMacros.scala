package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.runtime.{ArgumentLists, Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PartialTransformerDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, Const[ToPath, Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldConstPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ConstPartial[ToPath, Cfg], Flags]]
      }.applyFromSelector(selector)
    )
  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, Computed[ToPath, Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ComputedPartial[ToPath, Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, RenamedFrom[FromPath, ToPath, Cfg], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withCoproductInstanceImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Inst: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedInstance: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[
        From,
        To,
        CaseComputed[Path.Match[FixedInstance, Path.Root], Cfg],
        Flags
      ]]
  }.applyJavaEnumFixFromClosureSignature[Inst](f)

  def withCoproductInstancePartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Inst: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedInstance: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[
        From,
        To,
        CaseComputedPartial[Path.Match[FixedInstance, Path.Root], Cfg],
        Flags
      ]]
  }.applyJavaEnumFixFromClosureSignature[Inst](f)

  def withConstructorImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, Constructor[Args, Path.Root, Cfg], Flags]]
  }.applyFromBody(f)

  def withConstructorPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, ConstructorPartial[Args, Path.Root, Cfg], Flags]]
  }.applyFromBody(f)
}
