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
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, Const[ToPath, Overrides], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldConstPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ConstPartial[ToPath, Overrides], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, Computed[ToPath, Overrides], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ComputedFrom[FromPath, ToPath, Overrides], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ComputedPartial[ToPath, Overrides], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedPartialFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, ComputedPartialFrom[FromPath, ToPath, Overrides], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, RenamedFrom[FromPath, ToPath, Overrides], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withSealedSubtypeHandledImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedSubtype: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[
        From,
        To,
        CaseComputed[Path.SourceMatching[Path.Root, FixedSubtype], Overrides],
        Flags
      ]]
  }.applyJavaEnumFixFromClosureSignature[Subtype](f)

  def withSealedSubtypeHandledPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedSubtype: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[
        From,
        To,
        CaseComputedPartial[Path.SourceMatching[Path.Root, FixedSubtype], Overrides],
        Flags
      ]]
  }.applyJavaEnumFixFromClosureSignature[Subtype](f)

  def withSealedSubtypeRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromSubtype: WeakTypeTag,
      ToSubtype: WeakTypeTag
  ]: Tree = c.prefix.tree
    .asInstanceOfExpr(
      weakTypeTag[PartialTransformerDefinition[
        From,
        To,
        RenamedTo[Path.SourceMatching[Path.Root, FromSubtype], Path.Matching[Path.Root, ToSubtype], Overrides],
        Flags
      ]]
    )

  def withConstructorImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, Constructor[Args, Path.Root, Overrides], Flags]]
  }.applyFromBody(f)

  def withConstructorToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr(
        new ApplyFieldNameType {
          def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
            weakTypeTag[PartialTransformerDefinition[From, To, Constructor[Args, ToPath, Overrides], Flags]]
        }.applyFromSelector(selector)
      )
  }.applyFromBody(f)

  def withConstructorPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, ConstructorPartial[Args, Path.Root, Overrides], Flags]]
  }.applyFromBody(f)

  def withConstructorPartialToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr(
        new ApplyFieldNameType {
          def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
            weakTypeTag[PartialTransformerDefinition[From, To, ConstructorPartial[Args, ToPath, Overrides], Flags]]
        }.applyFromSelector(selector)
      )
  }.applyFromBody(f)

  def withConstructorEitherImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(q"_root_.io.scalaland.chimney.internal.runtime.FunctionEitherToResult.lift($f)")
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, ConstructorPartial[Args, Path.Root, Overrides], Flags]]
  }.applyFromBody(f)

  def withConstructorEitherToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(q"_root_.io.scalaland.chimney.internal.runtime.FunctionEitherToResult.lift($f)")
      .asInstanceOfExpr(
        new ApplyFieldNameType {
          def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
            weakTypeTag[PartialTransformerDefinition[From, To, ConstructorPartial[Args, ToPath, Overrides], Flags]]
        }.applyFromSelector(selector)
      )
  }.applyFromBody(f)
}
