package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionForAllMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree =
    TransformerDefinitionForAllDsl
      .withFieldRenamed[From, To, Overrides, Flags, FromMatch, ToMatch](
        c.Expr[io.scalaland.chimney.dsl.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]](
          c.prefix.tree
        ),
        anyExpr(selectorFrom),
        anyExpr(selectorTo)
      )
      .toUntypedResult

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionForAllDsl
      .withFieldConst[From, To, Overrides, Flags, FromMatch, ToMatch](
        c.Expr[io.scalaland.chimney.dsl.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]](
          c.prefix.tree
        ),
        anyExpr(selector),
        anyExpr(value)
      )
      .toUntypedResult

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionForAllDsl
      .withFieldComputed[From, To, Overrides, Flags, FromMatch, ToMatch](
        c.Expr[io.scalaland.chimney.dsl.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]](
          c.prefix.tree
        ),
        anyExpr(selector),
        anyExpr(f)
      )
      .toUntypedResult

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromMatch: WeakTypeTag,
      ToMatch: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionForAllDsl
      .withFieldComputedPartial[From, To, Overrides, Flags, FromMatch, ToMatch](
        c.Expr[io.scalaland.chimney.dsl.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]](
          c.prefix.tree
        ),
        anyExpr(selector),
        anyExpr(f)
      )
      .toUntypedResult
}
