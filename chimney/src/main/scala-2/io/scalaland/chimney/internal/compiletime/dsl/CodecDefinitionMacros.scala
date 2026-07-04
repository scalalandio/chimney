package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.reflect.macros.whitebox

class CodecDefinitionMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldRenamedImpl[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag,
      EncodeOverrides <: TransformerOverrides: WeakTypeTag,
      DecodeOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorDomain: Tree, selectorDto: Tree): Tree =
    CodecDefinitionDsl
      .withFieldRenamed[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags](
        prefixAnyExpr,
        anyExpr(selectorDomain),
        anyExpr(selectorDto)
      )
      .toUntypedResult

  def withSealedSubtypeRenamedImpl[
      Domain: WeakTypeTag,
      Dto: WeakTypeTag,
      EncodeOverrides <: TransformerOverrides: WeakTypeTag,
      DecodeOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      DomainSubtype: WeakTypeTag,
      DtoSubtype: WeakTypeTag
  ]: Tree =
    CodecDefinitionDsl
      .withSealedSubtypeRenamed[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags](
        prefixAnyExpr,
        typeOf_??[DomainSubtype],
        typeOf_??[DtoSubtype]
      )
      .toUntypedResult
}
