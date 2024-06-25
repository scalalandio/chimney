package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.reflect.macros.whitebox

class IsoDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      FromOverrides <: TransformerOverrides: WeakTypeTag,
      ToOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[IsoDefinition[
            From,
            To,
            RenamedFrom[FromPath, ToPath, FromOverrides],
            RenamedFrom[ToPath, FromPath, ToOverrides],
            Flags
          ]]
      }.applyFromSelectors(selectorFrom, selectorTo)
    )
}
