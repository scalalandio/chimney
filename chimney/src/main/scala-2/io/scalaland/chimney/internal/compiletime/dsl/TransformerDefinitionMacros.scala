package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.internal.runtime.TransformerCfg.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.*

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[FromField <: String: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[TransformerDefinition[From, To, FieldConst[FromField, Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[FromField <: String: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[TransformerDefinition[From, To, FieldComputed[FromField, Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromField <: String: WeakTypeTag, ToField <: String: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[TransformerDefinition[From, To, FieldRelabelled[FromField, ToField, Cfg], Flags]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withCoproductInstanceImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Inst: WeakTypeTag
  ](f: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr[TransformerDefinition[From, To, CoproductInstance[Inst, To, Cfg], Flags]]
}
