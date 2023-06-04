package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney
import io.scalaland.chimney.internal.macros.TransformerMacros

import scala.annotation.unused
import scala.reflect.macros.blackbox

// TODO: move to io.scalaland.chimney.internal.compiletime.dsl
class TransformerBlackboxMacros(val c: blackbox.Context) extends TransformerMacros {

  import c.universe.*

  def buildTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      Flags: WeakTypeTag,
      ImplicitScopeFlags: WeakTypeTag
  ](@unused tc: c.Tree): c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]](
      buildDefinedTransformer[From, To, C, Flags, ImplicitScopeFlags](DerivationTarget.TotalTransformer)
    )
  }

  def buildPartialTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      Flags: WeakTypeTag,
      ImplicitScopeFlags: WeakTypeTag
  ](@unused tc: c.Tree): c.Expr[chimney.PartialTransformer[From, To]] = {
    c.Expr[chimney.PartialTransformer[From, To]](
      buildDefinedTransformer[From, To, C, Flags, ImplicitScopeFlags](DerivationTarget.PartialTransformer())
    )
  }

  def transformImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ImplicitScopeFlags: WeakTypeTag
  ](tc: c.Tree): c.Expr[To] = {
    c.Expr[To](
      expandTransform[From, To, C, InstanceFlags, ImplicitScopeFlags](DerivationTarget.TotalTransformer, tc) {
        (derivedTransformer, srcField) =>
          derivedTransformer.callTransform(srcField)
      }
    )
  }

  def partialTransformNoFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ImplicitScopeFlags: WeakTypeTag
  ](tc: c.Tree): c.Expr[To] = {
    c.Expr[To](
      expandTransform[From, To, C, InstanceFlags, ImplicitScopeFlags](DerivationTarget.PartialTransformer(), tc) {
        (derivedTransformer, srcField) =>
          derivedTransformer.callPartialTransform(srcField, q"false")
      }
    )
  }

  def partialTransformFailFastImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ImplicitScopeFlags: WeakTypeTag
  ](tc: c.Tree): c.Expr[To] = {
    c.Expr[To](
      expandTransform[From, To, C, InstanceFlags, ImplicitScopeFlags](DerivationTarget.PartialTransformer(), tc) {
        (derivedTransformer, srcField) =>
          derivedTransformer.callPartialTransform(srcField, q"true")
      }
    )
  }

  def deriveTransformerImpl[From: WeakTypeTag, To: WeakTypeTag]: c.Expr[chimney.Transformer[From, To]] = {
    deriveWithTarget[From, To, chimney.Transformer[From, To]](DerivationTarget.TotalTransformer)
  }

  def derivePartialTransformerImpl[From: WeakTypeTag, To: WeakTypeTag]: c.Expr[chimney.PartialTransformer[From, To]] = {
    deriveWithTarget[From, To, chimney.PartialTransformer[From, To]](DerivationTarget.PartialTransformer())
  }
}
