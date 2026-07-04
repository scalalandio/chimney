package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.annotation.unused
import scala.reflect.macros.whitebox

class PartialTransformerIntoMacros(ctx: whitebox.Context) extends DslBundle(ctx) {

  import c.universe.{Tree, WeakTypeTag}

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldConst[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(value))
      .toUntypedResult

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldComputed[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withFieldComputedFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
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
    PartialTransformerIntoDsl
      .withFieldRenamed[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom), anyExpr(selectorTo))
      .toUntypedResult

  def withFieldUnusedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldUnused[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorFrom))
      .toUntypedResult

  def withSealedSubtypeHandledImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree =
    PartialTransformerIntoDsl
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
    PartialTransformerIntoDsl
      .withSealedSubtypeRenamed[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromSubtype], typeOf_??[ToSubtype])
      .toUntypedResult

  def withSealedSubtypeUnmatchedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    PartialTransformerIntoDsl
      .withSealedSubtypeUnmatched[From, To, Overrides, Flags](prefixExpr, anyExpr(selectorTo))
      .toUntypedResult

  def withFallbackImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](fallback: Tree): Tree =
    PartialTransformerIntoDsl
      .withFallback[From, To, Overrides, Flags](prefixExpr, typeOf_??[FromFallback], anyExpr(fallback))
      .toUntypedResult

  def withFallbackFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      FromFallback: WeakTypeTag
  ](selectorFrom: Tree)(fallback: Tree): Tree =
    PartialTransformerIntoDsl
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
    PartialTransformerIntoDsl
      .withConstructor[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorTo[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withSourceFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree): Tree =
    partialTransformerIntoWithSourceFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.PartialTransformerInto[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorFrom)
    ).toUntypedResult

  def withTargetFlagImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorTo: Tree): Tree =
    partialTransformerIntoWithTargetFlag[From, To, Overrides, Flags](
      c.Expr[io.scalaland.chimney.dsl.PartialTransformerInto[From, To, Overrides, Flags]](c.prefix.tree),
      anyExpr(selectorTo)
    ).toUntypedResult

  def withFieldConstPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldConstPartial[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(value))
      .toUntypedResult

  def withFieldComputedPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldComputedPartial[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withFieldComputedPartialFromImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldComputedPartialFrom[From, To, Overrides, Flags](
        prefixExpr,
        anyExpr(selectorFrom),
        anyExpr(selectorTo),
        anyExpr(f)
      )
      .toUntypedResult

  def withFieldComputedPartialFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldComputedPartialFailFast[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withFieldComputedPartialFromFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selectorFrom: Tree)(selectorTo: Tree, f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withFieldComputedPartialFromFailFast[From, To, Overrides, Flags](
        prefixExpr,
        anyExpr(selectorFrom),
        anyExpr(selectorTo),
        anyExpr(f)
      )
      .toUntypedResult

  def withSealedSubtypeHandledPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree =
    PartialTransformerIntoDsl
      .withSealedSubtypeHandledPartial[From, To, Overrides, Flags](
        prefixExpr,
        anyExpr(f),
        typeOf_??[Subtype]
      )
      .toUntypedResult

  def withSealedSubtypeHandledPartialFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag,
      Subtype: WeakTypeTag
  ](f: Tree): Tree =
    PartialTransformerIntoDsl
      .withSealedSubtypeHandledPartialFailFast[From, To, Overrides, Flags](
        prefixExpr,
        anyExpr(f),
        typeOf_??[Subtype]
      )
      .toUntypedResult

  def withConstructorPartialImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorPartial[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorPartialToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorPartialTo[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withConstructorPartialFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorPartialFailFast[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorPartialToFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorPartialToFailFast[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult

  def withConstructorEitherImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorEither[From, To, Overrides, Flags](prefixExpr, anyExpr(f))
      .toUntypedResult

  def withConstructorEitherToImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: TransformerOverrides: WeakTypeTag,
      Flags <: TransformerFlags: WeakTypeTag
  ](selector: Tree)(f: Tree)(@unused ev: Tree): Tree =
    PartialTransformerIntoDsl
      .withConstructorEitherTo[From, To, Overrides, Flags](prefixExpr, anyExpr(selector), anyExpr(f))
      .toUntypedResult
}
