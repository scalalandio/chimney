package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.internal.runtime.Path.*
import io.scalaland.chimney.internal.runtime.TransformerCfg.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PartialTransformerDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

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
          weakTypeTag[PartialTransformerDefinition[From, To, FieldConst[Select[FromField, Root], Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldConstPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(value)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[FromField <: String: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, FieldConstPartial[Select[FromField, Root], Cfg], Flags]]
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
          weakTypeTag[PartialTransformerDefinition[From, To, FieldComputed[Select[FromField, Root], Cfg], Flags]]
      }.applyFromSelector(selector)
    )

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = c.prefix.tree
    .addOverride(f)
    .asInstanceOfExpr(
      new ApplyFieldNameType {
        def apply[FromField <: String: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[PartialTransformerDefinition[From, To, FieldComputedPartial[Select[FromField, Root], Cfg], Flags]]
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
          weakTypeTag[PartialTransformerDefinition[
            From,
            To,
            FieldRelabelled[Select[FromField, Root], Select[ToField, Root], Cfg],
            Flags
          ]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )

  def withCoproductInstanceImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Inst: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedInstance: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[From, To, CoproductInstance[FixedInstance, To, Cfg], Flags]]
  }.applyJavaEnumFixFromClosureSignature[Inst](f)

  def withCoproductInstancePartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: TransformerCfg: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Inst: WeakTypeTag
  ](f: Tree): Tree = new ApplyFixedCoproductType {
    def apply[FixedInstance: WeakTypeTag]: Tree = c.prefix.tree
      .addOverride(f)
      .asInstanceOfExpr[PartialTransformerDefinition[
        From,
        To,
        CoproductInstancePartial[FixedInstance, To, Cfg],
        Flags
      ]]
  }.applyJavaEnumFixFromClosureSignature[Inst](f)
}
