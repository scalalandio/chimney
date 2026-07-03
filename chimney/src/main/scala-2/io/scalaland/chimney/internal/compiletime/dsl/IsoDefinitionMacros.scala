package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.reflect.macros.whitebox

class IsoDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldRenamedImpl[
      First: WeakTypeTag,
      Second: WeakTypeTag,
      FirstOverrides <: TransformerOverrides: WeakTypeTag,
      SecondOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFirst: Tree, selectorSecond: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FirstPath <: Path: WeakTypeTag, SecondPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[IsoDefinition[
            First,
            Second,
            Renamed[FirstPath, SecondPath, FirstOverrides],
            Renamed[SecondPath, FirstPath, SecondOverrides],
            Flags
          ]]
      }.applyFromSelectors(selectorFirst, selectorSecond)
    )

  def withSealedSubtypeRenamedImpl[
      First: WeakTypeTag,
      Second: WeakTypeTag,
      FirstOverrides <: TransformerOverrides: WeakTypeTag,
      SecondOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FirstSubtype: WeakTypeTag,
      SecondSubtype: WeakTypeTag
  ]: Tree = c.prefix.tree
    .asInstanceOfExpr(
      weakTypeTag[IsoDefinition[
        First,
        Second,
        Renamed[
          Path.SourceMatching[Path.Root, FirstSubtype],
          Path.Matching[Path.Root, SecondSubtype],
          FirstOverrides
        ],
        Renamed[
          Path.SourceMatching[Path.Root, SecondSubtype],
          Path.Matching[Path.Root, FirstSubtype],
          SecondOverrides
        ],
        Flags
      ]]
    )
}
