package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.runtime.{ArgumentLists, Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

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
          weakTypeTag[TransformerDefinition[From, To, Const[ToPath, Overrides], Flags]]
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
          weakTypeTag[TransformerDefinition[From, To, Computed[Path.Root, ToPath, Overrides], Flags]]
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
          weakTypeTag[TransformerDefinition[From, To, Computed[FromPath, ToPath, Overrides], Flags]]
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
          weakTypeTag[TransformerDefinition[From, To, Renamed[FromPath, ToPath, Overrides], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withFieldUnusedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[FromPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[TransformerDefinition[From, To, Unused[FromPath, Overrides], Flags]]
      }.applyFromSelector(selectorFrom)
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
      .asInstanceOfExpr[TransformerDefinition[
        From,
        To,
        Computed[Path.SourceMatching[Path.Root, FixedSubtype], Path.Root, Overrides],
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
      weakTypeTag[TransformerDefinition[
        From,
        To,
        Renamed[Path.SourceMatching[Path.Root, FromSubtype], Path.Matching[Path.Root, ToSubtype], Overrides],
        Flags
      ]]
    )

  def withSealedSubtypeUnmatchedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[TransformerDefinition[From, To, Unmatched[ToPath, Overrides], Flags]]
      }.applyFromSelector(selectorTo)
    )

  def withFallbackImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](fallback: Tree): Tree = c.prefix.tree
    .addOverride(fallback)
    .asInstanceOfExpr[TransformerDefinition[From, To, Fallback[FromFallback, Path.Root, Overrides], Flags]]

  def withFallbackFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](selectorFrom: Tree)(fallback: Tree): Tree = c.prefix.tree
    .addOverride(fallback)
    .asInstanceOfExpr(new ApplyFieldNameType {
      def apply[FromPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[TransformerDefinition[From, To, Fallback[FromFallback, FromPath, Overrides], Flags]]
    }.applyFromSelector(selectorFrom))

  def withConstructorImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree = new ApplyConstructorType {
    def apply[Args <: ArgumentLists: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[TransformerDefinition[From, To, Constructor[Args, Path.Root, Overrides], Flags]]
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
            weakTypeTag[TransformerDefinition[From, To, Constructor[Args, ToPath, Overrides], Flags]]
        }.applyFromSelector(selector)
      )
  }.applyFromBody(f)

  def withSourceFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree = {
    val pathFrom = new ApplyFieldNameType {
      def apply[FromPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] = weakTypeTag[FromPath]
    }.applyFromSelector(selectorFrom)
    q"""new _root_.io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfTransformerDefinition[${weakTypeOf[
        From
      ]}, ${weakTypeOf[To]}, ${weakTypeOf[Overrides]}, ${weakTypeOf[Flags]}, $pathFrom](${c.prefix.tree})"""
  }

  def withTargetFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree = {
    val pathTo = new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] = weakTypeTag[ToPath]
    }.applyFromSelector(selectorTo)
    q"""new _root_.io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfTransformerDefinition[${weakTypeOf[
        From
      ]}, ${weakTypeOf[To]}, ${weakTypeOf[Overrides]}, ${weakTypeOf[Flags]}, $pathTo](${c.prefix.tree})"""
  }
}
