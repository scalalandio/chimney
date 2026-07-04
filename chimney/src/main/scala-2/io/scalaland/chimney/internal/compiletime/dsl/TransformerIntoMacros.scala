package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerIntoMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    TransformerIntoDsl
      .withFieldConst[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(value))
      .toUntypedResult

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerIntoDsl
      .withFieldComputed[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withFieldComputedFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree =
    TransformerIntoDsl
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
    TransformerIntoDsl
      .withFieldRenamed[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom), anyExpr(selectorTo))
      .toUntypedResult

  def withFieldUnusedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    TransformerIntoDsl
      .withFieldUnused[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom))
      .toUntypedResult

  def withSealedSubtypeHandledImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree =
    TransformerIntoDsl
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
    TransformerIntoDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromSubtype], typeOf_??[ToSubtype])
      .toUntypedResult

  def withSealedSubtypeUnmatchedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    TransformerIntoDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorTo))
      .toUntypedResult

  def withFallbackImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](fallback: Tree): Tree =
    TransformerIntoDsl
      .withFallback[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromFallback], anyExpr(fallback))
      .toUntypedResult

  def withFallbackFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](selectorFrom: Tree)(fallback: Tree): Tree =
    TransformerIntoDsl
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
    TransformerIntoDsl
      .withConstructor[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    TransformerIntoDsl
      .withConstructorTo[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withSourceFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    transformerIntoWithSourceFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.TransformerInto[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorFrom)
    ).toUntypedResult

  def withTargetFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    transformerIntoWithTargetFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.TransformerInto[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorTo)
    ).toUntypedResult
}
