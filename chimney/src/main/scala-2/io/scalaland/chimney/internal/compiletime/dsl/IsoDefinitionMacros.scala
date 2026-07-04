package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.reflect.macros.whitebox

class IsoDefinitionMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldRenamedImpl[
      First: WeakTypeTag,
      Second: WeakTypeTag,
      FirstOverrides <: TransformerOverrides: WeakTypeTag,
      SecondOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFirst: Tree, selectorSecond: Tree): Tree =
    IsoDefinitionDsl
      .withFieldRenamed[First, Second, FirstOverrides, SecondOverrides, Flags](
        prefixAnyExpr,
        anyExpr(selectorFirst),
        anyExpr(selectorSecond)
      )
      .toUntypedResult

  def withSealedSubtypeRenamedImpl[
      First: WeakTypeTag,
      Second: WeakTypeTag,
      FirstOverrides <: TransformerOverrides: WeakTypeTag,
      SecondOverrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FirstSubtype: WeakTypeTag,
      SecondSubtype: WeakTypeTag
  ]: Tree =
    IsoDefinitionDsl
      .withSealedSubtypeRenamed[First, Second, FirstOverrides, SecondOverrides, Flags](
        prefixAnyExpr,
        typeOf_??[FirstSubtype],
        typeOf_??[SecondSubtype]
      )
      .toUntypedResult
}
