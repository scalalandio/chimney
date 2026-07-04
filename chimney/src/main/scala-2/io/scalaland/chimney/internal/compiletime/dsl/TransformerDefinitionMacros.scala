package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionDsl
      .withFieldConst[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(value))
      .toUntypedResult

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionDsl
      .withFieldComputed[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withFieldComputedFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionDsl
      .withFieldComputedFrom[From, To, Overrides, Flags](
        prefixExpr,
        anyExpr(selectorFrom),
        anyExpr(selectorTo),
        anyExpr(f)
      )
      .toUntypedResult

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree =
    TransformerDefinitionDsl
      .withFieldRenamed[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom), anyExpr(selectorTo))
      .toUntypedResult

  def withFieldUnusedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    TransformerDefinitionDsl
      .withFieldUnused[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom))
      .toUntypedResult

  def withSealedSubtypeHandledImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree =
    TransformerDefinitionDsl
      .withSealedSubtypeHandled[From, To, Overrides, Flags](prefixExpr, anyExpr(f), typeOf_??[Subtype])
      .toUntypedResult

  def withSealedSubtypeRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromSubtype: WeakTypeTag,
      ToSubtype: WeakTypeTag
  ]: Tree =
    TransformerDefinitionDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromSubtype], typeOf_??[ToSubtype])
      .toUntypedResult

  def withSealedSubtypeUnmatchedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    TransformerDefinitionDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorTo))
      .toUntypedResult

  def withFallbackImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](fallback: Tree): Tree =
    TransformerDefinitionDsl
      .withFallback[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromFallback], anyExpr(fallback))
      .toUntypedResult

  def withFallbackFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](selectorFrom: Tree)(fallback: Tree): Tree =
    TransformerDefinitionDsl
      .withFallbackFrom[From, To, Overrides, Flags](
        prefixExpr,
        typeOf_??[FromFallback],
        anyExpr(selectorFrom),
        anyExpr(fallback)
      )
      .toUntypedResult

  def withConstructorImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionDsl
      .withConstructor[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    TransformerDefinitionDsl
      .withConstructorTo[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withSourceFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    transformerDefinitionWithSourceFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.TransformerDefinition[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorFrom)
    ).toUntypedResult

  def withTargetFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    transformerDefinitionWithTargetFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.TransformerDefinition[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorTo)
    ).toUntypedResult
}
