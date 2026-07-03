package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.reflect.macros.whitebox

class CodecDefinitionMacros(val c: whitebox.Context) extends utils.DslMacroUtils {

  import c.universe.{Select as _, *}

  def withFieldRenamedImpl[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag,
      EncodeOverrides <: TransformerOverrides: WeakTypeTag,
      DecodeOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorDomain: Tree, selectorDto: Tree): Tree = c.prefix.tree
    .asInstanceOfExpr(
      new ApplyFieldNameTypes {
        def apply[FromPath <: Path: WeakTypeTag, ToPath <: Path: WeakTypeTag]: c.WeakTypeTag[?] =
          weakTypeTag[CodecDefinition[
            Domain,
            Dto,
            Renamed[FromPath, ToPath, EncodeOverrides],
            Renamed[ToPath, FromPath, DecodeOverrides],
            Flags
          ]]
      }.applyFromSelectors(selectorDomain, selectorDto)
    )

  def withSealedSubtypeRenamedImpl[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag,
      EncodeOverrides <: TransformerOverrides: WeakTypeTag,
      DecodeOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      DomainSubtype: WeakTypeTag,
      DtoSubtype: WeakTypeTag
  ]: Tree = c.prefix.tree
    .asInstanceOfExpr(
      weakTypeTag[CodecDefinition[
        Domain,
        Dto,
        Renamed[
          Path.SourceMatching[Path.Root, DomainSubtype],
          Path.Matching[Path.Root, DtoSubtype],
          EncodeOverrides
        ],
        Renamed[
          Path.SourceMatching[Path.Root, DtoSubtype],
          Path.Matching[Path.Root, DomainSubtype],
          DecodeOverrides
        ],
        Flags
      ]]
    )
}
