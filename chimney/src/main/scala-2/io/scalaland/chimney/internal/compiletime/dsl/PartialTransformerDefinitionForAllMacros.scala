package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PartialTransformerDefinitionForAllMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree =
    q"""
      new _root_.io.scalaland.chimney.dsl.PartialTransformerDefinition[
        ${weakTypeOf[From]},
        ${weakTypeOf[To]},
        _root_.io.scalaland.chimney.internal.runtime.TransformerOverrides.ForAll[
          ${weakTypeOf[FromMatch]},
          ${weakTypeOf[ToMatch]},
          ${new ApplyFieldNameTypes {
      def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[RenamedFrom[FromPath, ToPath, Empty]]
    }.applyFromSelectors(selectorFrom, selectorTo)},
          ${weakTypeOf[Overrides]}
        ],
        ${weakTypeOf[Flags]}
      ](${c.prefix}.runtimeData)
    """

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    q"""
      new _root_.io.scalaland.chimney.dsl.PartialTransformerDefinition[
        ${weakTypeOf[From]},
        ${weakTypeOf[To]},
        _root_.io.scalaland.chimney.internal.runtime.TransformerOverrides.ForAll[
          ${weakTypeOf[FromMatch]},
          ${weakTypeOf[ToMatch]},
          ${new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[Const[ToPath, Empty]]
    }.applyFromSelector(selector)},
          ${weakTypeOf[Overrides]}
        ],
        ${weakTypeOf[Flags]}
      ]($value +: ${c.prefix}.runtimeData)
    """

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    q"""
      new _root_.io.scalaland.chimney.dsl.PartialTransformerDefinition[
        ${weakTypeOf[From]},
        ${weakTypeOf[To]},
        _root_.io.scalaland.chimney.internal.runtime.TransformerOverrides.ForAll[
          ${weakTypeOf[FromMatch]},
          ${weakTypeOf[ToMatch]},
          ${new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[Computed[ToPath, Empty]]
    }.applyFromSelector(selector)},
          ${weakTypeOf[Overrides]}
        ],
        ${weakTypeOf[Flags]}
      ]($f +: ${c.prefix}.runtimeData)
    """

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    q"""
      new _root_.io.scalaland.chimney.dsl.PartialTransformerDefinition[
        ${weakTypeOf[From]},
        ${weakTypeOf[To]},
        _root_.io.scalaland.chimney.internal.runtime.TransformerOverrides.ForAll[
          ${weakTypeOf[FromMatch]},
          ${weakTypeOf[ToMatch]},
          ${new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[ComputedPartial[ToPath, Empty]]
    }.applyFromSelector(selector)},
          ${weakTypeOf[Overrides]}
        ],
        ${weakTypeOf[Flags]}
      ]($f +: ${c.prefix}.runtimeData)
    """
}
