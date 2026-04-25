package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerIntoForAllMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = {
    val overridesType = new ApplyFieldNameTypes {
      def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[ForAll[FromMatch, ToMatch, RenamedFrom[FromPath, ToPath, Empty], Overrides]]
    }.applyFromSelectors(selectorFrom, selectorTo)
    q"""
      new _root_.io.scalaland.chimney.dsl.TransformerInto[
        ${weakTypeOf[From]},
        ${weakTypeOf[To]},
        $overridesType,
        ${weakTypeOf[Flags]}
      ](
        ${c.prefix}.source,
        ${c.prefix}.td.asInstanceOf[_root_.io.scalaland.chimney.dsl.TransformerDefinition[
          ${weakTypeOf[From]},
          ${weakTypeOf[To]},
          $overridesType,
          ${weakTypeOf[Flags]}
        ]]
      )
    """
  }

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
    val overridesType = new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[ForAll[FromMatch, ToMatch, Const[ToPath, Empty], Overrides]]
    }.applyFromSelector(selector)
    q"""
      {
        val updatedTd = _root_.io.scalaland.chimney.internal.runtime.WithRuntimeDataStore
          .update(${c.prefix}.td, $value)
          .asInstanceOf[_root_.io.scalaland.chimney.dsl.TransformerDefinition[
            ${weakTypeOf[From]},
            ${weakTypeOf[To]},
            $overridesType,
            ${weakTypeOf[Flags]}
          ]]
        new _root_.io.scalaland.chimney.dsl.TransformerInto[
          ${weakTypeOf[From]},
          ${weakTypeOf[To]},
          $overridesType,
          ${weakTypeOf[Flags]}
        ](
          ${c.prefix}.source,
          updatedTd
        )
      }
    """
  }

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
    val overridesType = new ApplyFieldNameType {
      def apply[ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
        weakTypeTag[ForAll[FromMatch, ToMatch, Computed[ToPath, Empty], Overrides]]
    }.applyFromSelector(selector)
    q"""
      {
        val updatedTd = _root_.io.scalaland.chimney.internal.runtime.WithRuntimeDataStore
          .update(${c.prefix}.td, $f)
          .asInstanceOf[_root_.io.scalaland.chimney.dsl.TransformerDefinition[
            ${weakTypeOf[From]},
            ${weakTypeOf[To]},
            $overridesType,
            ${weakTypeOf[Flags]}
          ]]
        new _root_.io.scalaland.chimney.dsl.TransformerInto[
          ${weakTypeOf[From]},
          ${weakTypeOf[To]},
          $overridesType,
          ${weakTypeOf[Flags]}
        ](
          ${c.prefix}.source,
          updatedTd
        )
      }
    """
  }
}
